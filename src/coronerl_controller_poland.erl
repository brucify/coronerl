-module(coronerl_controller_poland).

%% API
-export([ get/2
        , post/2
        ]).

%%-define(POPULATION,
%%  [ {<<"Dolnośląskie">>, 2899986
%%  [ {<<"Kujawsko-pomorskie">>, 2074517
%%  [ {<<"Lubelskie">>, 2112216
%%  [ {<<"Lubuskie">>, 1013031
%%  [ {<<"Łódzkie">>, 2460170
%%  [ {<<"Małopolskie">>, 3404863
%%  [ {<<"Mazowieckie">>, 5411446
%%  [ {<<"Opolskie">>, 984345
%%  [ {<<"Podkarpackie">>, 2127462
%%  [ {<<"Podlaskie">>, 1179430
%%  [ {<<"Pomorskie">>, 2337769
%%  [ {<<"Śląskie">>, 4524091
%%  [ {<<"Świętokrzyskie">>, 1237369
%%  [ {<<"Warmińsko-mazurskie">>, 1425967
%%  [ {<<"Wielkopolskie">>, 3495470
%%  [ {<<"Zachodniopomorskie">>, 1698344
%%).

-define(POPULATION,
  [ {<<68,111,108,110,111,197,155,108,196,133,115,107,105,101>>, 2899986}  % <<"Dolnośląskie">>
  , {<<"Kujawsko-pomorskie">>, 2074517}
  , {<<"Lubelskie">>, 2112216}
  , {<<"Lubuskie">>, 1013031}
  , {<<"Mazowieckie">>, 5411446}
  , {<<77,97,197,130,111,112,111,108,115,107,105,101>>, 3404863}  % <<"Mazowieckie">>
  , {<<"Opolskie">>, 984345}
  , {<<"Podkarpackie">>, 2127462}
  , {<<"Podlaskie">>, 1179430}
  , {<<"Pomorskie">>, 2337769}
  , {<<87,97,114,109,105,197,132,115,107,111,45,77,97,122,117,
    114,115,107,105,101>>, 1425967}  % <<"Warmińsko-mazurskie">>
  , {<<"Wielkopolskie">>, 3495470}
  , {<<"Zachodniopomorskie">>, 1698344}
  , {<<197,129,195,179,100,122,107,105,101>>, 2460170}  % <<"Łódzkie">>
  , {<<197,154,108,196,133,115,107,105,101>>, 4524091}  % <<"Śląskie">>
  , {<<197,154,119,105,196,153,116,111,107,114,122,121,115,
    107,105,101>>, 1237369}  % <<"Świętokrzyskie">>
  ]
).

get(_Params, _State) ->
  Is = coronerl_csv_poland:match_incremental_for_provinces(infection),
  Ds = coronerl_csv_poland:match_incremental_for_provinces(death),
  Rs = coronerl_csv_poland:match_incremental_for_provinces(recovery),
  Result = #{ days => coronerl_csv_poland:lookup_timeline()
            , numbers => [ province(P, Is, Ds, Rs) || P <- coronerl_csv_poland:all_unique_provinces()]
            , provinces => coronerl_csv_poland:all_unique_provinces()
            },
  {continue, Result}.

post(_Params, _State) ->
  Result = coronerl_csv_poland:reset(),
  {continue, Result}.

-spec province(binary(),
               [{binary(), [integer()]}],
               [{binary(), [integer()]}],
               [{binary(), [integer()]}]) -> map().
province(Province, Is, Ds, Rs) ->
  {_, Ints} = hd(Is),
  Default = [0 || _ <- lists:seq(1, length(Ints), 1)],
  ConfirmedIncr = proplists:get_value(Province, Is, Default),
  DeathsIncr    = proplists:get_value(Province, Ds, Default),
  RecoveredIncr = proplists:get_value(Province, Rs, Default),
  Confirmed     = cumulative(ConfirmedIncr),
  Deaths        = cumulative(DeathsIncr),
  Recovered     = cumulative(RecoveredIncr),
  Active        = coronerl_global:active(Confirmed, Deaths, Recovered),
  #{ name            => Province
   , confirmed       => Confirmed
   , death           => Deaths
   , recovered       => Recovered
   , active          => Active
   , confirmed_daily => ConfirmedIncr
   , death_daily     => DeathsIncr
   , recovered_daily => RecoveredIncr
   , net_daily       => coronerl_global:incremental(Active)
   , population      => proplists:get_value(Province, ?POPULATION, 0)
   }.

-spec cumulative([integer()]) -> [integer()].
cumulative(List) ->
  cumulative(lists:reverse(List), []).

cumulative([], Acc) ->
  Acc;
cumulative([_H|Rest]=L, Acc) ->
  cumulative(Rest, [lists:sum(L)|Acc]).



