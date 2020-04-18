%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2020 20:08
%%%-------------------------------------------------------------------
-module(coronerl_csv_usa).

%% API
-export([ init/0
        , reset/0
        , tab/1
        , match_province_cummulative/2
        , match_dates/0
        , select_all_provinces/0
        , to_integer/1
        ]).

-define(FILENAME_CONFIRMED_USA, "time_series_covid19_confirmed_US.csv").
-define(FILENAME_DEATH_USA,     "time_series_covid19_deaths_US.csv").

init() ->
  ets:new(tab(confirmed),  [named_table, ordered_set, public]),
  ets:new(tab(death),      [named_table, ordered_set, public]),
  read_csv().

reset() ->
  ets:delete_all_objects(tab(confirmed)),
  ets:delete_all_objects(tab(death)),
  read_csv().

tab(confirmed) -> list_to_atom(atom_to_list(?MODULE)++"_confirmed");
tab(death)     -> list_to_atom(atom_to_list(?MODULE)++"_death").

read_csv() ->
  read_csv(tab(confirmed), filename:join(file_path(),?FILENAME_CONFIRMED_USA)),
  read_csv(tab(death),     filename:join(file_path(),?FILENAME_DEATH_USA)).

file_path() ->
  {ok, Path} = application:get_env(coronerl, time_series_csv_file_path),
  Path.

%%%===================================================================
%%% csv format specfic code
%%%===================================================================

-spec read_csv(atom(), string()) -> ok.
read_csv(Tab, FilePath) ->
  {ok, CsvBin} = file:read_file(FilePath),
  List = csv:decode_binary(CsvBin, [{return, binary}]),
  save(Tab, List),
  ok.

save(Tab, List) ->
  Objs = lists:foldl(
    fun([_,_,_,_,_,Admin2,ProvinceState,_,_Lat,_Long,_|Rest], Acc) ->
         [{{Admin2, ProvinceState}, Rest} | Acc]
    end,
    [], List
  ),
  ets:insert(Tab, Objs).

-spec match_province_cummulative(confirmed|death, binary()) -> [integer()].
match_province_cummulative(Tab, Province) ->
  MatchPattern = {{'_', Province},'_'},
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
  Objs = ets:match_object(tab(confirmed),{{<<"Admin2">>, <<"Province_State">>}, '_'}),
  [BinList] = [Bins || {_, Bins} <- Objs],
  BinList.

-spec select_all_provinces() -> [binary()].
select_all_provinces() ->
  lists:filter(fun(X) -> X /= <<"Province_State">> end,
    lists:usort(ets:select(tab(confirmed),[
      { {{'_', '$1'}, '_'}
      , []
      , ['$1']
      }
    ]))
  ).

to_integer("") -> 0;
to_integer(null) -> 0;
to_integer(X) when is_list(X) -> list_to_integer(X);
to_integer(X) when is_binary(X) -> binary_to_integer(X);
to_integer(X) when is_integer(X) -> X.