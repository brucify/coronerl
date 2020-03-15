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
        , read_csv/0
        , country/2
        , match_country/1
        , match_dates/0
        ]).

-define(FILEPATH, "/Users/bruce/git/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv").

init() ->
  ets:new(?MODULE, [named_table, ordered_set, public]),
  read_csv().

-spec read_csv() -> [list()].
read_csv() ->
  {ok, CsvBin} = file:read_file(?FILEPATH),
  List = csv:decode_binary(CsvBin),
  save(List).

save(List) ->
  Objs = lists:foldl(
    fun([Province,Country,_,_|Rest], Acc) ->
         [{{Country,Province}, Rest} | Acc]
    end,
    [], List
  ),
  ets:insert(?MODULE, Objs).

-spec match_country(string()) -> [integer()].
match_country(Country) ->
  Objs = ets:match_object(?MODULE,{{Country, '_'},'_'}),
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
  Objs = ets:match_object(?MODULE,{{"Country/Region", "Province/State"},'_'}),
  [BinList] = [lists:map(fun(X)-> list_to_binary(X) end, Strings) || {_, Strings} <- Objs],
  BinList.

-spec country(string(), [list()]) -> [list()].
country(Country, List) ->
  [X || [_, Country |_]=X <- List].