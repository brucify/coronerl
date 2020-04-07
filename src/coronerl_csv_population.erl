%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2020 19:01
%%%-------------------------------------------------------------------
-module(coronerl_csv_population).
-author("bruce").

%% API
-export([ init/0
        , reset/0
        , match_country/1
        , country_name_diff/0
        ]).

-define(FILENAME_POPULATION, "worldometers.csv").

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

-define(JOHNS_HOPKINS_DIFF,  [ {<<"Diamond Princess">>,   2670}
                             , {<<"Kosovo">>,             1932774}
                             , {<<"MS Zaandam">>,         1432}
                             , {<<"West Bank and Gaza">>, 2900034 + 1918221}
                             ]).

init() ->
  ets:new(tab(worldometers),  [named_table, ordered_set, public]),
  read_csv().

reset() ->
  ets:delete_all_objects(tab(worldometers)),
  read_csv().

tab(worldometers) -> list_to_atom(atom_to_list(?MODULE)++"_worldometers").

read_csv() ->
  read_csv(tab(worldometers), filename:join(file_path(), ?FILENAME_POPULATION)).

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

save(Tab, List) ->
  Objs = lists:foldl(
    fun([_, Country, Population | _Rest], Acc) ->
      [ {Country, Population} | Acc ]
    end,
    [], List
  ),
  ets:insert(Tab, Objs).

-spec match_country(binary()) -> integer().
match_country(CountryJHU) ->
  TryMatch = fun(X) -> case ets:match_object(tab(worldometers), {X, '_'}) of
                         [] -> 0;
                         [{_, PopBin}] -> binary_to_integer(PopBin)
                       end
             end,
  lists:foldl(
    fun(_Fun, Acc) when Acc /= 0 -> Acc;
       (Fun, _Acc)               -> Fun()
    end,
    0,
    [ fun() -> TryMatch(CountryJHU) end
    , fun() -> TryMatch(proplists:get_value(CountryJHU, ?NAME_MAPPING)) end
    , fun() -> proplists:get_value(CountryJHU, ?JOHNS_HOPKINS_DIFF, 0) end
    ]
  ).

country_name_diff() ->
  L1 = ets:select(coronerl_csv_global:tab(confirmed), [{{{'$1','_'},'_'}, [],['$1']}]), % Johns Hopkins
  L2 = ets:select(tab(worldometers),                  [{{'$1','_'}, [],['$1']}]), % Worldometers
  Diff1 = lists:usort(L2) -- lists:usort(L1),
  Diff2 = lists:usort(L1) -- lists:usort(L2),
  Diff1 ++ Diff2.