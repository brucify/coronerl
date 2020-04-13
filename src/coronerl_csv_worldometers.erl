%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2020 19:01
%%%-------------------------------------------------------------------
-module(coronerl_csv_worldometers).
-author("bruce").

%% API
-export([ init/0
        , reset/0
        , match_country_population/1
        , match_country_land_area/1
        , country_name_diff/0
        ]).

-define(FILENAME_POPULATION, "worldometers_population.csv").
-define(FILENAME_AREA,       "worldometers_area.csv").

                    % {<<"Jonhs Hopkins">>, <<"Worldometers">>}
-define(NAME_MAPPING, [ {undefined,                    <<"American Samoa">>}
                      , {undefined,                    <<"Anguilla">>}
                      , {undefined,                    <<"Aruba">>}
                      , {undefined,                    <<"Bermuda">>}
                      , {undefined,                    <<"British Virgin Islands">>}
                      , {undefined,                    <<"Caribbean Netherlands">>}
                      , {undefined,                    <<"Cayman Islands">>}
                      , {undefined,                    <<"Channel Islands">>}
                      , {undefined,                    <<"Comoros">>}
                      , {<<"Congo (Brazzaville)">>,    <<"Congo">>}
                      , {undefined,                    <<"Cook Islands">>}
                      , {undefined,                    <<"Country (or dependency)">>}
                      , {undefined,                    <<"Curaçao"/utf8>>}
                      , {<<"Czechia">>,                <<"Czech Republic (Czechia)">>}
                      , {<<"Cote d'Ivoire">>,          <<"Côte d'Ivoire"/utf8>>}
                      , {<<"Congo (Kinshasa)">>,       <<"DR Congo">>}
                      , {undefined,                    <<"Faeroe Islands">>}
                      , {undefined,                    <<"Falkland Islands">>}
                      , {undefined,                    <<"French Guiana">>}
                      , {undefined,                    <<"French Polynesia">>}
                      , {undefined,                    <<"Gibraltar">>}
                      , {undefined,                    <<"Greenland">>}
                      , {undefined,                    <<"Guadeloupe">>}
                      , {undefined,                    <<"Guam">>}
                      , {undefined,                    <<"Hong Kong">>}
                      , {undefined,                    <<"Isle of Man">>}
                      , {undefined,                    <<"Kiribati">>}
                      , {undefined,                    <<"Lesotho">>}
                      , {undefined,                    <<"Macao">>}
                      , {undefined,                    <<"Marshall Islands">>}
                      , {undefined,                    <<"Martinique">>}
                      , {undefined,                    <<"Mayotte">>}
                      , {undefined,                    <<"Micronesia">>}
                      , {undefined,                    <<"Montserrat">>}
                      , {<<"Burma">>,                  <<"Myanmar">>}
                      , {undefined,                    <<"Nauru">>}
                      , {undefined,                    <<"New Caledonia">>}
                      , {undefined,                    <<"Niue">>}
                      , {undefined,                    <<"North Korea">>}
                      , {undefined,                    <<"Northern Mariana Islands">>}
                      , {undefined,                    <<"Palau">>}
                      , {undefined,                    <<"Puerto Rico">>}
                      , {undefined,                    <<"Réunion"/utf8>>}
                      , {undefined,                    <<"Saint Barthelemy">>}
                      , {undefined,                    <<"Saint Helena">>}
                      , {<<"Saint Kitts and Nevis">>,  <<"Saint Kitts & Nevis">>}
                      , {undefined,                    <<"Saint Martin">>}
                      , {undefined,                    <<"Saint Pierre & Miquelon">>}
                      , {undefined,                    <<"Samoa">>}
                      , {undefined,                    <<"Sao Tome & Principe">>}
                      , {undefined,                    <<"Sint Maarten">>}
                      , {undefined,                    <<"Solomon Islands">>}
                      , {<<"Korea, South">>,           <<"South Korea">>}
                      , {<<"Saint Vincent and the Grenadines">>, <<"St. Vincent & Grenadines">>}
                      , {undefined,                    <<"State of Palestine">>}
                      , {<<"Taiwan*">>,                <<"Taiwan">>}
                      , {undefined,                    <<"Tajikistan">>}
                      , {undefined,                    <<"Tokelau">>}
                      , {undefined,                    <<"Tonga">>}
                      , {undefined,                    <<"Turkmenistan">>}
                      , {undefined,                    <<"Turks and Caicos">>}
                      , {undefined,                    <<"Tuvalu">>}
                      , {undefined,                    <<"U.S. Virgin Islands">>}
                      , {<<"US">>,                     <<"United States">>}
                      , {undefined,                    <<"Vanuatu">>}
                      , {undefined,                    <<"Wallis & Futuna">>}
                      , {undefined,                    <<"Yemen">>}
                      ]).

-define(JOHNS_HOPKINS_POP_DIFF, [ {<<"Diamond Princess">>,   2670}
                                , {<<"MS Zaandam">>,         1432}
                                , {<<"Kosovo">>,             1932774}
                                , {<<"West Bank and Gaza">>, 2900034 + 1918221}
                                ]).

-define(JOHNS_HOPKINS_LANDAREA_DIFF, [ {<<"Diamond Princess">>,   1}
                                     , {<<"MS Zaandam">>,         1}
                                     , {<<"Kosovo">>,             10887}
                                     , {<<"West Bank and Gaza">>, 5640 + 360}
                                     ]).


init() ->
  ets:new(tab(population),  [named_table, ordered_set, public]),
  ets:new(tab(area),        [named_table, ordered_set, public]),
  read_csv().

reset() ->
  ets:delete_all_objects(tab(population)),
  ets:delete_all_objects(tab(area)),
  read_csv().

tab(population) -> list_to_atom(atom_to_list(?MODULE)++"_population");
tab(area)       -> list_to_atom(atom_to_list(?MODULE)++"_area").

read_csv() ->
  read_csv(population, filename:join(file_path(), ?FILENAME_POPULATION)),
  read_csv(area,       filename:join(file_path(), ?FILENAME_AREA)).

file_path() ->
  Path = code:priv_dir("coronerl"),
  Path.

-spec read_csv(atom(), string()) -> [list()].
read_csv(Tab, FilePath) ->
  {ok, CsvBin} = file:read_file(FilePath),
  List = csv:decode_binary(CsvBin, [{return, binary}]),
  save(Tab, List).

%%%===================================================================
%%% csv format specfic code
%%%===================================================================

save(area, List) ->
  Objs = lists:foldl(
    fun([_, Country, _TotAreaKm, _TotAreaMi, LandAreaKm, _LandAreaMi | _Rest], Acc) ->
      [ {Country, LandAreaKm} | Acc ]
    end,
    [], List
  ),
  ets:insert(tab(area), Objs);
save(population, List) ->
  Objs = lists:foldl(
    fun([_, Country, Population | _Rest], Acc) ->
      [ {Country, Population} | Acc ]
    end,
    [], List
  ),
  ets:insert(tab(population), Objs).

-spec match_country_population(binary()) -> integer().
match_country_population(CountryJHU) ->
  match_country(population, CountryJHU).

-spec match_country_land_area(binary()) -> integer().
match_country_land_area(CountryJHU) ->
  match_country(area, CountryJHU).

-spec match_country(atom(), binary()) -> integer().
match_country(Tab, CountryJHU) ->
  TryMatch = fun(X) -> case ets:match_object(tab(Tab), {X, '_'}) of
                         [] -> 1;
                         [{_, Bin}] -> binary_to_integer(Bin)
                       end
             end,
  lists:foldl(
    fun(_Fun, Acc) when Acc /= 1 -> Acc;
       (Fun, _Acc)               -> Fun()
    end,
    1,
    [ fun() -> TryMatch(CountryJHU) end
    , fun() -> TryMatch(proplists:get_value(CountryJHU, ?NAME_MAPPING)) end
    , fun() -> proplists:get_value(CountryJHU, diff(Tab), 1) end
    ]
  ).

diff(population) -> ?JOHNS_HOPKINS_POP_DIFF;
diff(area)       -> ?JOHNS_HOPKINS_LANDAREA_DIFF.

country_name_diff() ->
  L1 = ets:select(coronerl_csv_global:tab(confirmed), [{{{'$1','_'},'_'}, [],['$1']}]), % Johns Hopkins
  L2 = ets:select(tab(population),                    [{{'$1',      '_'}, [],['$1']}]), % Worldometers
  Diff1 = lists:usort(L2) -- lists:usort(L1),
  Diff2 = lists:usort(L1) -- lists:usort(L2),
  Diff1 ++ Diff2.