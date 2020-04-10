-module(coronerl_global_id_controller).

%% API
-export([ get/2
        ]).

get(Params, _State) ->
  CountryIds = parse_ids(proplists:get_value(id, Params)),
  Result =
    case lists:filter(fun(X) -> X /= undefined end,
      [coronerl_csv_global:lookup_country(Id) || Id <- CountryIds]
    ) of
      [] -> #{};
      CountryNames ->
        #{ days => coronerl_csv_global:match_dates()
         , numbers => [coronerl_global:country(Name) || Name <- CountryNames]
         }
    end,
  {continue, Result}.

-spec parse_ids(binary()) -> [integer()].
parse_ids(Ids) ->
  [list_to_integer(I) || I <- string:tokens(binary_to_list(Ids),",")].