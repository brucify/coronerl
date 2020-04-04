-module(default_handler).

-behavior(cowboy_rest).

%% cowboy callbacks
-export([ init/2
        , allowed_methods/2
        , content_types_provided/2
        , content_types_accepted/2
        , delete_resource/2
        ]).

%% my callbacks
-export([ handle_get/2
        , handle_put/2
        , handle_post/2
        ]).

-record(state, { controller_module :: atom()
               , allowed_methods :: [atom()]
               , request_params = [] :: [{atom(), any()}]
               , request_body :: binary()
               }).

%%%===================================================================
%%% Cowboy REST callbacks
%%%===================================================================

init(Req, [ControllerModule, AllowedMethods]) ->
  {cowboy_rest, Req, #state{ controller_module = ControllerModule
                           , allowed_methods = AllowedMethods
                           }}.

allowed_methods(Req, #state{allowed_methods = AllowedMethods}=State) ->
  {get_allowed_methods(AllowedMethods), Req, State}.

%%
%% GET and HEAD methods
%%
content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

%%
%% PUT, POST and PATCH methods
%%
content_types_accepted(#{method:=<<"PUT">>}=Req, State) ->
  {[{<<"application/json">>, handle_put}], Req, State};
content_types_accepted(#{method:=<<"POST">>}=Req, State) ->
  {[ {<<"application/json">>, handle_post}
   , {<<"application/x-www-form-urlencoded">>, handle_post}
   , {<<"text/plain">>, handle_post}
   ], Req, State}.

%%
%% DELETE method
%%
delete_resource(Req0, #state{controller_module = Controller}=State) ->
  {Status, RespBody, Headers} = Controller:delete(Req0, State),
  Req1 = do_cowboy_reply(Req0, Status, RespBody, Headers),
  {stop, Req1, State}.

%%%===================================================================
%%% Custom Cowboy REST callbacks
%%%===================================================================

handle_get(Req, State)->
  fold_steps(Req, State, [ fun parse_path_params/2
                         , fun parse_query_params/2
                         , fun parse_body_params/2
                         , fun parse_headers/2
                         , fun type_check_params/2
                         , fun call_controller/2
                         ]).

handle_put(#{path:=_Path}=Req, State)->
  fold_steps(Req, State, [ fun parse_path_params/2
                         , fun parse_query_params/2
                         , fun parse_body_params/2
                         , fun type_check_params/2
                         , fun call_controller/2
                         ]).

handle_post(#{path:=_Path}=Req, State)->
  fold_steps(Req, State, [ fun parse_path_params/2
                         , fun parse_query_params/2
                         , fun parse_body_params/2
                         , fun type_check_params/2
                         , fun call_controller/2
                         ]).


%%%===================================================================
%%% Steps
%%%===================================================================

call_controller(#{method := Method}= Req0, #state{ controller_module = Controller
                                                 , request_params = ReqParams
                                                 , request_body = Body
                                                 }=State) ->
  Fun = method_to_fun(Method),
  Params = maybe_add_plain_text_body(Body, ReqParams, Req0),

  try Controller:Fun(Params, State) of
    {continue, RespBody} ->
      EncodedBody = coronerl_json:encode(RespBody),
      Req1 = cowboy_req:set_resp_headers(#{}, Req0),
      case Method of
        <<"GET">> ->
          Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
          {EncodedBody, Req2, State};
        _Other    ->
          {true, cowboy_req:set_resp_body(EncodedBody, Req1), State}
      end;
    {Status, RespBody, RespHeaders} ->
      Req1 = do_cowboy_reply(Req0, Status, RespBody, RespHeaders),
      {stop, Req1, State}
  catch
    exit:400 ->
      Req1 = do_cowboy_reply(Req0, 400, <<"Bad request">>, #{}),
      {stop, Req1, State};
    exit:{Status, Msg} ->
      Req1 = do_cowboy_reply(Req0, Status, Msg, #{}),
      {stop, Req1, State}
  end.

type_check_params(Req, #state{request_params = Params}=State) ->
  {continue, Req, State#state{request_params = Params}}.

parse_headers(Req, #state{request_params = Params}=State) ->
  {continue, Req, State#state{request_params = Params}}.

parse_body_params(#{}=Req0, #state{request_params = Params}=State) ->
  {ok, BodyBin, Req1} = read_body(Req0),
  case maybe_decode_body_json(BodyBin, Req1) of
    {error, _Reason} ->
      Req2 = do_cowboy_reply(Req1, 400, <<"Invalid json">>, #{}),
      {stop, Req2, State};
    {ok, DecodedBody} when is_map(DecodedBody) ->
      BodyParams = atomize_keys(maps:to_list(DecodedBody)),
      {continue, Req0, State#state{request_params = BodyParams++Params}};
    {ok, DecodedBody} ->
      {continue, Req0, State#state{request_body = DecodedBody}}
  end.

parse_path_params(#{bindings := Bindings}=Req0, #state{request_params = Params}=State) ->
  PathParams = atomize_keys(maps:to_list(Bindings)),
  {continue, Req0, State#state{request_params = PathParams++Params}}.

parse_query_params(Req, #state{request_params = Params}=State) ->
  AtomsQs = atomize_keys(cowboy_req:parse_qs(Req)),
  {continue, Req, State#state{request_params = AtomsQs++Params}}.

%%%===================================================================
%%% Internal
%%%===================================================================

fold_steps(Req0, State0, AllFuns) ->
  lists:foldl(
    fun(Fun, {continue, Req, State}) -> Fun(Req, State);
       ([],  {continue, Req, State}) -> {true, Req, State};
       (_Fun,          CowboyReturn) -> CowboyReturn
    end,
    {continue, Req0, State0},
    AllFuns
  ).

do_cowboy_reply(Req0, Status, RespBody, Headers) ->
  Req1 = cowboy_req:reply( Status
                         , Headers#{<<"content-type">> => <<"application/json">>}
                         , coronerl_json:encode(RespBody)
                         , Req0
                         ),
  Req1.

-spec get_allowed_methods([atom()]) -> [binary()].
get_allowed_methods(Atoms) ->
  lists:map(fun('GET')    -> <<"GET">>;
               ('POST')   -> <<"POST">>;
               ('PUT')    -> <<"PUT">>;
               ('DELETE') -> <<"DELETE">>
            end,
    Atoms).

method_to_fun(<<"GET">>)    -> get;
method_to_fun(<<"POST">>)   -> post;
method_to_fun(<<"PUT">>)    -> put;
method_to_fun(<<"DELETE">>) -> delete.

atomize_keys(PropList) ->
  [{binary_to_existing_atom(K, latin1), V} || {K, V} <- PropList].

read_body(Req0) ->
  case read_body(Req0, <<>>) of
    {ok, <<>>, Req1}    -> {ok, <<"{}">>, Req1};
    {ok, BodyBin, Req1} -> {ok, BodyBin, Req1}
  end.

%% Read the entire body
read_body(Req0, Acc) ->
  case cowboy_req:read_body(Req0) of
    {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
    {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
  end.

-spec maybe_decode_body_json(binary(), cowboy_req:req()) -> {ok, map() | binary()} | {error, term()}.
maybe_decode_body_json(BodyBin, Req0) ->
  case cowboy_req:header(<<"content-type">>, Req0) of
    <<"application/json">> -> coronerl_json:decode(BodyBin);
    <<"text/plain">>       -> {ok, BodyBin};
    undefined              -> {ok, BodyBin}
  end.

maybe_add_plain_text_body(Body, ReqParams, Req0) ->
  case cowboy_req:header(<<"content-type">>, Req0) of
    undefined              -> ReqParams;
    <<"application/json">> -> ReqParams;
    <<"text/plain">>       -> [{body, Body}| ReqParams]
  end.