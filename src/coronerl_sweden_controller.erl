-module(coronerl_sweden_controller).

%% API
-export([ get/2
        , post/2
        ]).

get(_Params, _State) ->
  Result =
    #{ days => coronerl_csv_sweden:match_dates()
     , numbers => provinces()
     },
  {continue, Result}.

post(_Params, _State) ->
  Result = coronerl_csv_sweden:reset(),
  {continue, Result}.

provinces() ->
  List = coronerl_csv_sweden:match_all_provinces_incremental(),
  [ #{ name            => list_to_binary(Province)
     , confirmed_daily => lists:reverse(Integers)
     , confirmed       => cummulative(Integers)
     }
  || {Province, Integers} <- List
  ].

cummulative(Integers) ->
  cummulative(Integers, []).

cummulative([], Acc) ->
  Acc;
cummulative([_|Rest]=List, Acc) ->
  cummulative(Rest, [lists:sum(List) | Acc]).