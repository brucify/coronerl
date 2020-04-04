-module(coronerl_controller).

%% API
-export([ get/2
        , post/2
        , put/2
        , delete/2
        ]).

get(_Params, _State) ->
  Result =
    #{ days => coronerl_csv:match_dates()
     , numbers =>
         [ country("Sweden",         10099265)
         , country("Denmark",         5792202)
         , country("Norway",          5421241)
         , country("Finland",         5540720)
         , country("Iceland",          341243)
         , country("United Kingdom", 67886011)
         , country("Portugal",       10196709)
         , country("Spain",          46754778)
         , country("Italy",          60461826)
         , country("France",         65273511)
         , country("Belgium",        11589623)
         , country("Netherlands",    17134872)
         , country("Germany",        83783942)
         , country("Switzerland",     8654622)
         , country("Austria",         9006398)
         , country("Slovenia",        2078938)
         , country("Hungary",         9660351)
         , country("Slovakia",        5459642)
         , country("Czechia",        10708981)
         , country("Poland",         37846611)
         , country("Korea, South",   51269185)
         , country("Japan",         126476461)
         , country("US",            331002651)
         , country("Canada",         37742154)
         , country("Brazil",        212559417)
         , country("Russia",        145934462)
         %, country("India",        1380004385)
         , country("China",        1439323776)
         , country("South Africa",   59308690)
         , country("Australia",      25499884)
         , country("Iran",           83992949)
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

-spec country(string(), integer()) -> map().
country(CountryName, Population) ->
  {ConfirmedPadded, DeathsPadded, RecoveredPadded} =
    pad_with_nulls(coronerl_csv:match_country(confirmed, CountryName),
                   coronerl_csv:match_country(death,     CountryName),
                   coronerl_csv:match_country(recovered, CountryName)),
  Active = lists:zipwith(
    fun(X,Y) when X==null orelse Y==null -> null;
       (X,Y)->coronerl_csv:to_integer(X)-coronerl_csv:to_integer(Y)
    end,
    ConfirmedPadded,
    lists:zipwith(
      fun(X,Y) when X==null orelse Y==null -> null;
         (X,Y)->coronerl_csv:to_integer(X)+coronerl_csv:to_integer(Y)
      end,
      DeathsPadded, RecoveredPadded
    )
  ),
  #{ country => list_to_binary(CountryName)
   , confirmed       => ConfirmedPadded
   , death           => DeathsPadded
   , recovered       => RecoveredPadded
   , active          => Active
   , confirmed_daily => daily_cases(ConfirmedPadded)
   , death_daily     => daily_cases(DeathsPadded)
   , recovered_daily => daily_cases(RecoveredPadded)
   , net_daily       => daily_cases(Active)
   , population      => Population
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