-module(coronerl_global_id_controller).

%% API
-export([ get/2
        ]).

get(Params, _State) ->
  CountryName = proplists:get_value(id, Params),
  Result =
    case coronerl_csv_global:match_country_cummulative(confirmed, CountryName) of
      [] -> #{};
      _  -> #{ days => coronerl_csv_global:match_dates()
             , numbers => [ coronerl_global:country(CountryName) ]
             }
    end,
  {continue, Result}.