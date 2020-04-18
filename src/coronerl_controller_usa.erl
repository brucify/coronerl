-module(coronerl_controller_usa).

%% API
-export([ get/2
        , post/2
        ]).

get(_Params, _State) ->
  Result = #{ days => coronerl_csv_usa:match_dates()
            , numbers =>
                [ province(P) || P <- coronerl_csv_usa:select_all_provinces() ]
            },
  {continue, Result}.

post(_Params, _State) ->
  Result = coronerl_csv_usa:reset(),
  {continue, Result}.

-spec province(binary()) -> map().
province(ProvinceName) ->
  [ConfirmedPadded, DeathsPadded] = coronerl_global:pad_with_nulls(
    [ coronerl_csv_usa:match_province_cummulative(confirmed, ProvinceName)
    , coronerl_csv_usa:match_province_cummulative(death,     ProvinceName)
    ]
  ),

  #{ name            => ProvinceName
   , confirmed       => ConfirmedPadded
   , death           => DeathsPadded
   , confirmed_daily => coronerl_global:incremental(ConfirmedPadded)
   , death_daily     => coronerl_global:incremental(DeathsPadded)
   , population      => coronerl_csv_usa:match_population(ProvinceName)
   }.
