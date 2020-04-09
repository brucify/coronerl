%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2020 21:44
%%%-------------------------------------------------------------------
-module(coronerl_global).
-author("bruce").

%% API
-export([ country/1
        , country/2
        ]).

-spec country(binary()) -> map().
country(CountryName) ->
  country(CountryName, true).

-spec country(binary(), boolean()) -> map().
country(CountryName, MergeProvinces) ->
  {ConfirmedPadded, DeathsPadded, RecoveredPadded} =
    pad_with_nulls(coronerl_csv_global:match_country_cummulative(confirmed, CountryName, MergeProvinces),
                   coronerl_csv_global:match_country_cummulative(death,     CountryName, MergeProvinces),
                   coronerl_csv_global:match_country_cummulative(recovered, CountryName, MergeProvinces)),
  Active = lists:zipwith(
    fun(X,Y) when X==null orelse Y==null -> null;
      (X,Y)-> coronerl_csv_global:to_integer(X)- coronerl_csv_global:to_integer(Y)
    end,
    ConfirmedPadded,
    lists:zipwith(
      fun(X,Y) when X==null orelse Y==null -> null;
        (X,Y)-> coronerl_csv_global:to_integer(X)+ coronerl_csv_global:to_integer(Y)
      end,
      DeathsPadded, RecoveredPadded
    )
  ),
  #{ name            => CountryName
   , confirmed       => ConfirmedPadded
   , death           => DeathsPadded
   , recovered       => RecoveredPadded
   , active          => Active
   , confirmed_daily => incremental(ConfirmedPadded)
   , death_daily     => incremental(DeathsPadded)
   , recovered_daily => incremental(RecoveredPadded)
   , net_daily       => incremental(Active)
   , population      => coronerl_csv_population:match_country(CountryName)
   }.

-spec incremental([integer()]) -> [integer()].
incremental(L) ->
  L1 = lists:sublist([0|L], length(L)),
  lists:zipwith(fun(X,Y) -> coronerl_csv_global:to_integer(X)- coronerl_csv_global:to_integer(Y) end, L, L1).

pad_with_nulls(L1, L2, L3) ->
  Null = null,
  MaxLen = lists:max([length(L1),length(L2),length(L3)]),
  { lists:append(L1, [Null || _ <- lists:seq(1, MaxLen-length(L1), 1)])
  , lists:append(L2, [Null || _ <- lists:seq(1, MaxLen-length(L2), 1)])
  , lists:append(L3, [Null || _ <- lists:seq(1, MaxLen-length(L3), 1)])
  }.