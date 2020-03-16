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
         , country("France")
         , country("Germany")
         , country("Spain")
         , country("Italy")
         , country("Korea, South")
         , country("Japan")
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
  Confirmed = coronerl_csv:match_country(confirmed, String),
  Recovered = coronerl_csv:match_country(recovered, String),
  #{ country => list_to_binary(String)
   , confirmed       => Confirmed
   , death           => coronerl_csv:match_country(death, String)
   , recovered       => Recovered
   , active          => lists:zipwith(fun(X,Y)->X-Y end, Confirmed, Recovered)
   , confirmed_daily => daily_cases(Confirmed)
   }.

-spec daily_cases([integer()]) -> [integer()].
daily_cases(L) ->
  L1 = lists:sublist([0|L], length(L)),
  lists:zipwith(fun(X,Y) -> X-Y end, L, L1).