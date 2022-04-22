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
        , match_population/1
        , select_all_provinces/0
        ]).

%% CSV data located at
%% https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
-define(FILENAME_CONFIRMED_USA, "time_series_covid19_confirmed_US.csv").
-define(FILENAME_DEATH_USA,     "time_series_covid19_deaths_US.csv").

init() ->
  ets:new(tab(population), [named_table, ordered_set, public]),
  ets:new(tab(confirmed),  [named_table, ordered_set, public]),
  ets:new(tab(death),      [named_table, ordered_set, public]),
  read_csv().

reset() ->
  ets:delete_all_objects(tab(population)),
  ets:delete_all_objects(tab(confirmed)),
  ets:delete_all_objects(tab(death)),
  read_csv().

tab(population) -> list_to_atom(atom_to_list(?MODULE)++"_population");
tab(confirmed) -> list_to_atom(atom_to_list(?MODULE)++"_confirmed");
tab(death)     -> list_to_atom(atom_to_list(?MODULE)++"_death").

read_csv() ->
  read_csv(confirmed, filename:join(file_path(),?FILENAME_CONFIRMED_USA)),
  read_csv(death,     filename:join(file_path(),?FILENAME_DEATH_USA)).

file_path() ->
  {ok, Path} = application:get_env(coronerl, time_series_csv_file_path),
  Path.

%%%===================================================================
%%% csv format specfic code
%%%===================================================================

-spec read_csv(confirmed|death, string()) -> ok.
read_csv(Tab, FilePath) ->
  {ok, CsvBin} = file:read_file(FilePath),
  List = csv:decode_binary(CsvBin, [{return, binary}]),
  save(Tab, List),
  ok.

save(death, List) ->
  {L1, L2} = lists:foldl(
    fun([_,_,_,_,_,Admin2,ProvinceState,_,_Lat,_Long,_,Population|Rest], {Acc1, Acc2}) ->
      { [{{Admin2, ProvinceState}, Rest}       | Acc1]
      , [{{Admin2, ProvinceState}, Population} | Acc2]
      }
    end,
    {[], []}, List
  ),
  ets:insert(tab(death),      L1),
  ets:insert(tab(population), L2);
save(confirmed, List) ->
  Objs = lists:foldl(
    fun([_,_,_,_,_,Admin2,ProvinceState,_,_Lat,_Long,_|Rest], Acc) ->
         [{{Admin2, ProvinceState}, Rest} | Acc]
    end,
    [], List
  ),
  ets:insert(tab(confirmed), Objs).

-spec match_province_cummulative(confirmed|death, binary()) -> [integer()].
match_province_cummulative(Tab, Province) ->
  Objs = ets:match_object(tab(Tab), {{'_', Province},'_'}),
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

-spec match_population(binary()) -> integer().
match_population(Province) ->
  Objs = ets:match_object(tab(population), {{'_', Province},'_'}),
  case [to_integer(X) || {_, X} <- Objs] of
    [] -> 1;
    L  -> lists:sum(L)
  end.

-spec select_all_provinces() -> [binary()].
select_all_provinces() ->
  lists:filter(fun(X) -> not ( (X =:= <<"Province_State">>)
                            or (X =:= <<"Diamond Princess">>)
                            or (X =:= <<"Grand Princess">>)
                             )
               end,
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