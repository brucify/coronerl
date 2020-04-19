-module(coronerl_controller_sweden).

%% API
-export([ get/2
        , post/2
        ]).

-define(POPULATION,
  [ {"Blekinge",          159837}
  , {"Dalarna",           287350}
  , {"Gotland",            59253}
  , {"Gävleborg",         286774}
  , {"Halland",           330310}
  , {"Jämtland",          130517}
  , {"Jönköping",         361759}
  , {"Kalmar",            244856}
  , {"Kronoberg",         200252}
  , {"Norrbotten",        250533}
  , {"Skåne",            1365964}
  , {"Stockholm",        2352549}
  , {"Södermanland",      295459}
  , {"Uppsala",           378246}
  , {"Värmland",          281646}
  , {"Västerbotten",      270528}
  , {"Västernorrland",    245371}
  , {"Västmanland",       274516}
  , {"Västra_Götaland",  1713907}
  , {"Örebro",            303096}
  , {"Östergötland",      462379}
  , {"Riket",           10255102}
  ]
).

-define(LAND_AREA,
  [ {"Stockholm", 6513.83}
  , {"Uppsala", 8189.28}
  , {"Södermanland", 6071.83}
  , {"Östergötland", 10557.43}
  , {"Jönköping", 10436.45}
  , {"Kronoberg", 8423.28}
  , {"Kalmar", 11159.80}
  , {"Gotland", 3134.44}
  , {"Blekinge", 2931.25}
  , {"Skåne", 10965.04}
  , {"Halland", 5426.59}
  , {"Västra_Götaland", 23800.38}
  , {"Värmland", 17519.24}
  , {"Örebro", 8503.99}
  , {"Västmanland", 5117.28}
  , {"Dalarna", 28030.12}
  , {"Gävleborg", 18113.25}
  , {"Västernorrland", 21548.49}
  , {"Jämtland", 48935.24}
  , {"Västerbotten", 54664.43}
  , {"Norrbotten", 97241.87}
  ]
).

get(_Params, _State) ->
  Result =
    #{ days => coronerl_csv_sweden:match_dates()
      %% warning mixed keys under 'numbers'
     , numbers => provinces()
               ++ death_vs_icu()
     },
  {continue, Result}.

post(_Params, _State) ->
  Result = coronerl_csv_sweden:reset(),
  {continue, Result}.

provinces() ->
  List = coronerl_csv_sweden:match_all_provinces_incremental(),
  [ #{ name            => Province
     , confirmed_daily => lists:reverse(Integers)
     , confirmed       => cummulative(Integers)
     , population      => get_value(Province, ?POPULATION)
     , land_area       => get_value(Province, ?LAND_AREA)
     }
  || {Province, Integers} <- List
  ].

death_vs_icu() ->
  List = coronerl_csv_sweden:match_all_kumulativa(),
  [ #{ name            => SerieName
     , death_vs_icu    => lists:reverse(Integers)
     , death_vs_icu_daily => coronerl_global:incremental(lists:reverse(Integers))
     , population      => proplists:get_value("Riket", ?POPULATION)
     , land_area       => lists:sum([X || {_, X} <- ?LAND_AREA])
     }
  || {SerieName, Integers} <- List
  ].

cummulative(Integers) ->
  cummulative(Integers, []).

cummulative([], Acc) ->
  Acc;
cummulative([_|Rest]=List, Acc) ->
  cummulative(Rest, [lists:sum(List) | Acc]).

%% Do characters_to_binary on both keys
-spec get_value(binary(), [{binary(), number()}]) -> integer().
get_value(ProvinceName, Proplist) ->
  proplists:get_value(unicode:characters_to_binary(ProvinceName),
    [{unicode:characters_to_binary(K), V} || {K,V} <- Proplist]
  ).