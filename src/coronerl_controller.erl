-module(coronerl_controller).

%% API
-export([ get/2
        , post/2
        , put/2
        , delete/2
        ]).

get(_Params, _State) ->
  Result =
    #{ months => coronerl_csv:match_dates()
     , numbers =>
         [ country("Sweden")
         , country("Denmark")
         , country("Norway")
         , country("Finland")
         , country("Iceland")
         , country("France")
         , country("Germany")
         , country("Spain")
         , country("Italy")
         , country("Korea, South")
         , country("Japan")
         , country("United Kingdom")
         , country("US")
         ]
     },
  {continue, Result}.

post(_Params, _State) ->
  Result = coronerl_csv:reset(),
  {continue, Result}.

put(_Params, _State) ->
  Result = <<"hello world">>,
  {continue, Result}.

delete(_Params, _State) ->
  Result = <<"hello world">>,
  {continue, Result}.

country(String) ->
  {ConfirmedPadded, DeathsPadded, RecoveredPadded} =
    pad_with_nulls(coronerl_csv:match_country(confirmed, String),
                   coronerl_csv:match_country(death, String),
                   coronerl_csv:match_country(recovered, String)),
  #{ country => list_to_binary(String)
   , confirmed       => ConfirmedPadded
   , death           => DeathsPadded
   , recovered       => RecoveredPadded
   , active          => lists:zipwith(fun(X,Y) when X==null orelse Y==null -> null;
                                         (X,Y)->coronerl_csv:to_integer(X)-coronerl_csv:to_integer(Y)
                                      end,
                                      ConfirmedPadded,
                                      lists:zipwith(
                                        fun(X,Y) when X==null orelse Y==null -> null;
                                           (X,Y)->coronerl_csv:to_integer(X)+coronerl_csv:to_integer(Y)
                                        end,
                                        DeathsPadded, RecoveredPadded
                                      )
                        )
   , confirmed_daily => daily_cases(ConfirmedPadded)
   , death_daily     => daily_cases(DeathsPadded)
   }.

-spec daily_cases([integer()]) -> [integer()].
daily_cases(L) ->
  L1 = lists:sublist([0|L], length(L)),
  lists:zipwith(fun(X,Y) -> coronerl_csv:to_integer(X)-coronerl_csv:to_integer(Y) end, L, L1).

pad_with_nulls(L1, L2, L3) ->
  Null = null,
  MaxLen = lists:max([length(L1),length(L2),length(L3)]),
  { lists:append(L1, [Null || _ <- lists:seq(1, MaxLen-length(L1), 1)])
  , lists:append(L2, [Null || _ <- lists:seq(1, MaxLen-length(L2), 1)])
  , lists:append(L3, [Null || _ <- lists:seq(1, MaxLen-length(L3), 1)])
  }.