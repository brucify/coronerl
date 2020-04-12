-module(coronerl_poland_controller).

%% API
-export([ get/2
        , post/2
        ]).

get(_Params, _State) ->
  Is = coronerl_csv_poland:match_incremental_for_provinces(infection),
  Ds = coronerl_csv_poland:match_incremental_for_provinces(death),
  Rs = coronerl_csv_poland:match_incremental_for_provinces(recovery),
  Result = #{ days => coronerl_csv_poland:lookup_timeline()
            , numbers => [ make_numbers(P, Is, Ds, Rs) || P <- coronerl_csv_poland:all_unique_provinces()]
            },
  {continue, Result}.

post(_Params, _State) ->
  Result = coronerl_csv_poland:reset(),
  {continue, Result}.

-spec make_numbers(
    binary(),
    [{binary(), [integer()]}],
    [{binary(), [integer()]}],
    [{binary(), [integer()]}]) -> map().
make_numbers(Province, Is, Ds, Rs) ->
  {_, Ints} = hd(Is),
  Default = [0 || _ <- lists:seq(1, length(Ints), 1)],
  ConfirmedIncr = proplists:get_value(Province, Is, Default),
  DeathsIncr    = proplists:get_value(Province, Ds, Default),
  RecoveredIncr = proplists:get_value(Province, Rs, Default),
  Confirmed     = cumulative(ConfirmedIncr),
  Deaths        = cumulative(DeathsIncr),
  Recovered     = cumulative(RecoveredIncr),
  Active        = coronerl_global:active(Confirmed, Deaths, Recovered),
  #{ name            => Province
   , confirmed       => Confirmed
   , death           => Deaths
   , recovered       => Recovered
   , active          => Active
   , confirmed_daily => ConfirmedIncr
   , death_daily     => DeathsIncr
   , recovered_daily => RecoveredIncr
   , net_daily       => coronerl_global:incremental(Active)
   , population      => undefined
   }.

-spec cumulative([integer()]) -> [integer()].
cumulative(List) ->
  cumulative(lists:reverse(List), []).

cumulative([], Acc) ->
  Acc;
cumulative([_H|Rest]=L, Acc) ->
  cumulative(Rest, [lists:sum(L)|Acc]).



