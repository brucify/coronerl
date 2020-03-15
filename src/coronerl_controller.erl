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
         [ country("Sweden")
         , country("Denmark")
         , country("Norway")
         , country("Finland")
         , country("Iceland")
         ]
     },
  {continue, Result}.

post(_Params, _State) ->
  Result = coronerl_csv:reset(),
  {continue, Result}.

put(_Params, _State) ->
  Result = <<"hello world">>,
  {continue, Result}.

delete(_Params, _State) ->
  Result = <<"hello world">>,
  {continue, Result}.

country(String) ->
  #{ country => list_to_binary(String)
   , confirmed => coronerl_csv:match_country(confirmed, String)
   , death     => coronerl_csv:match_country(death, String)
   , recovered => coronerl_csv:match_country(recovered, String)
   }.