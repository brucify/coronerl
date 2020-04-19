%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2020 09:58
%%%-------------------------------------------------------------------
-module(coronerl_csv_sweden).
-author("bruce").

%% API
-export([ init/0
        , reset/0
        , match_all_provinces_incremental/0
        , match_all_kumulativa/0
        , match_dates/0
        , to_integer/1
        ]).

-define(FILENAME_SWEDEN_REGION, "time_series_region.csv").

init() ->
  ets:new(tab(region),  [named_table, ordered_set, public]),
  read_csv().

reset() ->
  ets:delete_all_objects(tab(region)),
  read_csv().

tab(region) -> list_to_atom(atom_to_list(?MODULE)++"_region").

read_csv() ->
  read_csv(tab(region), filename:join(file_path(),?FILENAME_SWEDEN_REGION)).

file_path() ->
  {ok, Path} = application:get_env(coronerl, sweden_csv_file_path),
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
  Series = series(List, []),

  Objs = lists:foldl(
    fun([Province | Rest], Acc) ->
      [{Province, Rest} | Acc]
    end,
    [], Series
  ),
  ets:insert(Tab, Objs).

series([], Acc) ->
  Acc;
series(List, Acc) when hd(List) == [] ->
  Acc;
series(List0, Acc) ->
  OneSerie = [X    || [X |    _] <- List0],
  List1    = [Rest || [_ | Rest] <- List0],
  series(List1, [OneSerie|Acc]).

%%provinces(HeaderRow) ->
%%  [ "Totalt_antal_fall", "Kumulativa_fall" | Rest0 ] = HeaderRow,
%%  [ "Statistikdatum"
%%  , "Kumulativa_intensivvardade"
%%  , "Antal_intensivvardade"
%%  , "Kumulativa_avlidna"
%%  , "Antal_avlidna"
%%  | Rest1
%%  ] = lists:reverse(Rest0),
%%  lists:reverse(Rest1).

-spec match_all_provinces_incremental() -> [{binary(), [integer()]}].
match_all_provinces_incremental() ->
  Objs = ets:match_object(tab(region), {'_','_'}),
  Exclude = [ <<"Statistikdatum">>
            , <<"Kumulativa_intensivvardade">>
            , <<"Antal_intensivvardade">>
            , <<"Kumulativa_avlidna">>
            , <<"Antal_avlidna">>
            , <<"Kumulativa_fall">>
            , <<"Totalt_antal_fall">>
            ],

  KVs = [{Title, lists:map(fun(X)->to_integer(X) end, Numbers)} || {Title, Numbers} <- Objs
                                                                 , not lists:member(Title, Exclude)],
  KVs.

%% Kumulativa_fall
%% Kumulativa_avlidna
%% Kumulativa_intensivvardade
-spec match_all_kumulativa() -> [{string(), [integer()]}].
match_all_kumulativa() ->
  Objs = ets:select(tab(region), [ { {<<"Kumulativa_fall">>,           '_'}, [], ['$_'] }
                                 , { {<<"Kumulativa_avlidna">>,        '_'}, [], ['$_'] }
                                 , { {<<"Kumulativa_intensivvardade">>,'_'}, [], ['$_'] }
                                 ]),

  NumList = [{Title, lists:map(fun(X)->to_integer(X) end, Numbers)} || {Title, Numbers} <- Objs],
  NumList.

-spec match_dates() -> [binary()].
match_dates() ->
  Objs = ets:match_object(tab(region), {<<"Statistikdatum">>,'_'}),
  [BinList] = [Bins|| {_, Bins} <- Objs],
  lists:reverse(BinList).

to_integer("") -> 0;
to_integer(null) -> 0;
to_integer(X) when is_list(X) -> list_to_integer(X);
to_integer(X) when is_binary(X) -> binary_to_integer(X);
to_integer(X) when is_integer(X) -> X.