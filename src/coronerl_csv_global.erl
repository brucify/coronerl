%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2020 20:08
%%%-------------------------------------------------------------------
-module(coronerl_csv_global).

%% API
-export([ init/0
        , reset/0
        , tab/1
        , match_country_cummulative/2
        , match_country_cummulative/3
        , match_dates/0
        , to_integer/1
        ]).

-define(FILENAME_CONFIRMED, "time_series_covid19_confirmed_global.csv").
-define(FILENAME_DEATH,     "time_series_covid19_deaths_global.csv").
-define(FILENAME_RECOVERED, "time_series_covid19_recovered_global.csv").

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

tab(confirmed) -> list_to_atom(atom_to_list(?MODULE)++"_confirmed");
tab(death)     -> list_to_atom(atom_to_list(?MODULE)++"_death");
tab(recovered) -> list_to_atom(atom_to_list(?MODULE)++"_recovered").

read_csv() ->
  read_csv(tab(confirmed), filename:join(file_path(),?FILENAME_CONFIRMED)),
  read_csv(tab(death),     filename:join(file_path(),?FILENAME_DEATH)),
  read_csv(tab(recovered), filename:join(file_path(),?FILENAME_RECOVERED)).

file_path() ->
  {ok, Path} = application:get_env(coronerl, global_csv_file_path),
  Path.

%%%===================================================================
%%% csv format specfic code
%%%===================================================================

-spec read_csv(atom(), string()) -> [list()].
read_csv(Tab, FilePath) ->
  {ok, CsvBin} = file:read_file(FilePath),
  List = csv:decode_binary(CsvBin, [{return, binary}]),
  save(Tab, List).

save(Tab, List) ->
  Objs = lists:foldl(
    fun([Province,Country,_,_|Rest], Acc) ->
         [{{Country,Province}, Rest} | Acc]
    end,
    [], List
  ),
  ets:insert(Tab, Objs).

-spec match_country_cummulative(confirmed|death|recovered, binary()) -> [integer()].
match_country_cummulative(Tab, Country) ->
  match_country_cummulative(Tab, Country, true).

-spec match_country_cummulative(confirmed|death|recovered, binary(), boolean()) -> [integer()].
match_country_cummulative(Tab, Country, MergeProvinces) ->
  MatchPattern =
    case MergeProvinces of
      %% In case multiple provinces in one country
      true ->
        {{Country, '_'},'_'};
      false ->
        {{Country,  <<>>},'_'}
    end,
  Objs = ets:match_object(tab(Tab), MatchPattern),
  case [lists:map(fun(X)->to_integer(X) end, Numbers) || {_, Numbers} <- Objs] of
    [] -> [];
    NumList ->
      lists:foldl(
        fun(L, Acc) ->
          lists:zipwith(fun(X,Y)-> X+Y end, L, Acc)
        end,
        hd(NumList), tl(NumList)
      )
  end.

-spec match_dates() -> [binary()].
match_dates() ->
  Objs = ets:match_object(tab(confirmed),{{<<"Country/Region">>, <<"Province/State">>}, '_'}),
  [BinList] = [Bins || {_, Bins} <- Objs],
  BinList.

to_integer("") -> 0;
to_integer(null) -> 0;
to_integer(X) when is_list(X) -> list_to_integer(X);
to_integer(X) when is_binary(X) -> binary_to_integer(X);
to_integer(X) when is_integer(X) -> X.