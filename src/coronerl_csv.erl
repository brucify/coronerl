%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2020 20:08
%%%-------------------------------------------------------------------
-module(coronerl_csv).

%% API
-export([ init/0
        , reset/0
        , country/2
        , match_country/2
        , match_dates/0
        ]).

-define(FILEPATH, "/Users/bruce/git/COVID-19/csse_covid_19_data/csse_covid_19_time_series").
-define(FILEPATH_CONFIRMED, ?FILEPATH ++ "/time_series_19-covid-Confirmed.csv").
-define(FILEPATH_DEATH,     ?FILEPATH ++ "/time_series_19-covid-Deaths.csv").
-define(FILEPATH_RECOVERED, ?FILEPATH ++ "/time_series_19-covid-Recovered.csv").

init() ->
  ets:new(tab(confirmed),  [named_table, ordered_set, public]),
  ets:new(tab(death),      [named_table, ordered_set, public]),
  ets:new(tab(recovered),  [named_table, ordered_set, public]),
  read_csv().

reset() ->
  ets:delete_all_objects(tab(confirmed)),
  ets:delete_all_objects(tab(death)),
  ets:delete_all_objects(tab(recovered)),
  read_csv().

tab(confirmed) ->
  list_to_atom(atom_to_list(?MODULE)++"_confirmed");
tab(death) ->
  list_to_atom(atom_to_list(?MODULE)++"_death");
tab(recovered) ->
  list_to_atom(atom_to_list(?MODULE)++"_recovered").

read_csv() ->
  read_csv(tab(confirmed), ?FILEPATH_CONFIRMED),
  read_csv(tab(death),     ?FILEPATH_DEATH),
  read_csv(tab(recovered), ?FILEPATH_RECOVERED).

-spec read_csv(atom(), string()) -> [list()].
read_csv(Tab, FilePath) ->
  {ok, CsvBin} = file:read_file(FilePath),
  List = csv:decode_binary(CsvBin),
  save(Tab, List).

save(Tab, List) ->
  Objs = lists:foldl(
    fun([Province,Country,_,_|Rest], Acc) ->
         [{{Country,Province}, Rest} | Acc]
    end,
    [], List
  ),
  ets:insert(Tab, Objs).

-spec match_country(confirmed|death|recovered, string()) -> [integer()].
match_country(Tab, Country) ->
  Objs = ets:match_object(tab(Tab),{{Country, '_'},'_'}),
  NumList = [lists:map(fun(X)->list_to_integer(X) end, Numbers) || {_, Numbers} <- Objs],
  lists:foldl(
    fun(L, Acc) ->
      lists:zipwith(fun(X,Y)-> X+Y end,
                    L, Acc)
    end,
    hd(NumList), tl(NumList)
  ).

-spec match_dates() -> [binary()].
match_dates() ->
  Objs = ets:match_object(tab(confirmed),{{"Country/Region", "Province/State"},'_'}),
  [BinList] = [lists:map(fun(X)-> list_to_binary(X) end, Strings) || {_, Strings} <- Objs],
  BinList.

-spec country(string(), [list()]) -> [list()].
country(Country, List) ->
  [X || [_, Country |_]=X <- List].