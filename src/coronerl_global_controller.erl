-module(coronerl_global_controller).

%% API
-export([ get/2
        , post/2
        ]).

-export([ country/1
        ]).

get(_Params, _State) ->
  Result =
    #{ days => coronerl_csv_global:match_dates()
     , numbers =>
         [ country(<<"Sweden">>)
         , country(<<"Denmark">>)
         , country(<<"Norway">>)
         , country(<<"Finland">>)
         , country(<<"Iceland">>)
         , country(<<"United Kingdom">>)
         , country(<<"Portugal">>)
         , country(<<"Spain">>)
         , country(<<"Italy">>)
         , country(<<"France">>, false)
         , country(<<"Belgium">>)
         , country(<<"Netherlands">>)
         , country(<<"Germany">>)
         , country(<<"Switzerland">>)
         , country(<<"Austria">>)
         , country(<<"Slovenia">>)
         , country(<<"Hungary">>)
         , country(<<"Slovakia">>)
         , country(<<"Czechia">>)
         , country(<<"Poland">>)
         , country(<<"Korea, South">>)
         , country(<<"Japan">>)
         , country(<<"Taiwan*">>)
         , country(<<"Singapore">>)
         , country(<<"Canada">>)
         , country(<<"Brazil">>)
         , country(<<"Russia">>)
         %, country(<<"India">>)
         , country(<<"China">>)
         , country(<<"South Africa">>)
         , country(<<"Australia">>)
         , country(<<"Iran">>)
         ]
     },
  {continue, Result}.

post(_Params, _State) ->
  Result = coronerl_csv_global:reset(),
  {continue, Result}.

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