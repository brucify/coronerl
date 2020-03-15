-module(coronerl_controller).

%% API
-export([ get/2
        , post/2
        , put/2
        , delete/2
        ]).

get(_Params, _State) ->
  Result =
    #{ months => coronerl_csv:match_dates()
     , numbers =>
         [ confirmed("Sweden")
         , confirmed("Denmark")
         , confirmed("Norway")
         , confirmed("Finland")
         , confirmed("Iceland")
         ]
     },
  {continue, Result}.

post(_Params, _State) ->
  Result = <<"hello world">>,
  {continue, Result}.

put(_Params, _State) ->
  Result = <<"hello world">>,
  {continue, Result}.

delete(_Params, _State) ->
  Result = <<"hello world">>,
  {continue, Result}.

confirmed(String) ->
  #{ country => list_to_binary(String)
   , confirmed => coronerl_csv:match_country(String)
   }.