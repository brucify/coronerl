%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2020 09:58
%%%-------------------------------------------------------------------
-module(coronerl_csv_poland).
-author("bruce").

%% API
-export([ init/0
        , reset/0
        , lookup_timeline/0
        , all_unique_provinces/0
        , match_incremental_for_provinces/1
        ]).

-define(FILENAME_POLAND_TIMESERIES, "CoronavirusPL - Timeseries.csv").

init() ->
  ets:new(tab(infection), [named_table, ordered_set, public]),
  ets:new(tab(death),     [named_table, ordered_set, public]),
  ets:new(tab(recovery),  [named_table, ordered_set, public]),
  read_csv().

reset() ->
  ets:delete_all_objects(tab(infection)),
  ets:delete_all_objects(tab(death)),
  ets:delete_all_objects(tab(recovery)),
  read_csv().

tab(infection) -> list_to_atom(atom_to_list(?MODULE)++"_infection");
tab(death)     -> list_to_atom(atom_to_list(?MODULE)++"_death");
tab(recovery)  -> list_to_atom(atom_to_list(?MODULE)++"_recovery").

read_csv() ->
  FilePath = filename:join(file_path(),?FILENAME_POLAND_TIMESERIES),
  {ok, CsvBin} = file:read_file(FilePath),
  List = csv:decode_binary(CsvBin, [{return, binary}]),
  save(List).

file_path() ->
  {ok, Path} = application:get_env(coronerl, poland_csv_file_path),
  Path.

%%%===================================================================
%%% csv format specfic code
%%%===================================================================

save(CsvLines) ->
  Timeline = timeline(CsvLines),
%%  io:format(user, "~n Timeline: ~p ~n", [Timeline]),

  InfectionLines = [Line || [_Timestamp,_Province,_City,_PostalCode, <<"I">> | _Rest]=Line <- CsvLines],
  DeathLines     = [Line || [_Timestamp,_Province,_City,_PostalCode, <<"D">> | _Rest]=Line <- CsvLines],
  RecoveryLines  = [Line || [_Timestamp,_Province,_City,_PostalCode, <<"R">> | _Rest]=Line <- CsvLines],

  ProvinceCityI = lists:usort([{Province,City} || [_Timestamp,Province,City | _Rest] <- InfectionLines]),
  ProvinceCityD = lists:usort([{Province,City} || [_Timestamp,Province,City | _Rest] <- DeathLines]),
  ProvinceCityR = lists:usort([{Province,City} || [_Timestamp,Province,City | _Rest] <- RecoveryLines]),

  KV1 = [ {{Province, City}, incremental(Timeline, [L || [_,P,C | _]=L <- InfectionLines, P=:=Province, C=:=City])}
        || {Province, City} <- ProvinceCityI
        ],
  KV2 = [ {{Province, City}, incremental(Timeline, [L || [_,P,C | _]=L <- DeathLines,     P=:=Province, C=:=City])}
        || {Province, City} <- ProvinceCityD
        ],
  KV3 = [ {{Province, City}, incremental(Timeline, [L || [_,P,C | _]=L <- RecoveryLines,  P=:=Province, C=:=City])}
        || {Province, City} <- ProvinceCityR
        ],

  T = {timeline, Timeline},

  ets:insert(tab(infection), [T|KV1]),
  ets:insert(tab(death),     [T|KV2]),
  ets:insert(tab(recovery),  [T|KV3]).

-spec match_incremental_for_provinces(infection|death|recovery) -> [{binary(), [integer()]}].
match_incremental_for_provinces(Tab) ->
  Provinces = select_all_provinces(Tab),
  lists:map(
    fun(Province) ->
      Objs = ets:match_object(tab(Tab), {{Province, '_'}, '_'}),
      {Province, zip_numbers([Numbers || {_, Numbers} <- Objs])}
    end,
    Provinces
  ).

-spec all_unique_provinces() -> [binary()].
all_unique_provinces() ->
  lists:usort(
    select_all_provinces(infection) ++
    select_all_provinces(death) ++
    select_all_provinces(recovery)
  ).

-spec lookup_timeline() -> [binary()].
lookup_timeline() ->
  ets:lookup_element(tab(infection), timeline, 2).

-spec zip_numbers([[integer()]]) -> [integer()].
zip_numbers(NumLists) ->
  case NumLists of
    [] -> [];
    NumList ->
      lists:foldl(
        fun(L, Acc) ->
          lists:zipwith(fun(X,Y)-> X+Y end, L, Acc)
        end,
        hd(NumList), tl(NumList)
      )
  end.

select_all_provinces(Tab) ->
  lists:usort(ets:select(tab(Tab),[
    { {{'$1', '_'}, '_'}
    , []
    , ['$1']
    }
  ])).

incremental(Timeline, List) ->
  DataTimestamps = [Ts || [Ts | _Rest] <- List],
  L = lists:foldl(
    fun(Timestamp, Acc) ->
      Count = length([X || X <- DataTimestamps, X =:= Timestamp]),
      [Count|Acc]
    end,
    [], Timeline
  ),
  lists:reverse(L).

timeline(List) ->
  AllTimestamps = lists:sort(
    fun(X,Y) -> timestamp_to_gregorian_days(X) < timestamp_to_gregorian_days(Y) end,
    [Timestamp || [Timestamp | _Rest] <- List, Timestamp /= <<"Timestamp">>]
  ),
  DayZero = hd(AllTimestamps),
  DayLast = hd(lists:reverse(AllTimestamps)),
  Timeline =
    [ date_to_timestamp(calendar:gregorian_days_to_date(X))
      || X <- lists:seq(timestamp_to_gregorian_days(DayZero), timestamp_to_gregorian_days(DayLast), 1)
    ],
  Timeline.

%% <<"09-04-2020">>
timestamp_to_gregorian_days(<<DD:16, _:8, MM:16, _:8, YYYY:32>>) ->
  calendar:date_to_gregorian_days( binary_to_integer(<<YYYY:16>>)
                                 , binary_to_integer(<<MM:16>>)
                                 , binary_to_integer(<<DD:16>>)
                                 ).

date_to_timestamp({YY, M, D}) ->
  YYYY = integer_to_binary(YY + 2000),
  MM = pad_to_two_digits(M),
  DD = pad_to_two_digits(D),
  <<DD/binary, "-", MM/binary, "-", YYYY/binary>>.

pad_to_two_digits(Int) ->
  case integer_to_binary(Int) of
    X when byte_size(X) == 1 -> <<"0",X/binary>>;
    XX when byte_size(XX) == 2 -> XX
  end.