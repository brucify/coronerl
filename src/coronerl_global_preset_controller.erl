-module(coronerl_global_preset_controller).

%% API
-export([ get/2
        ]).

get(_Params, _State) ->
  Result =
    #{ days => coronerl_csv_global:match_dates()
     , numbers =>
         [ coronerl_global:country(<<"Sweden">>)
         , coronerl_global:country(<<"Denmark">>)
         , coronerl_global:country(<<"Norway">>)
         , coronerl_global:country(<<"Finland">>)
         , coronerl_global:country(<<"Iceland">>)
         , coronerl_global:country(<<"United Kingdom">>)
         , coronerl_global:country(<<"Portugal">>)
         , coronerl_global:country(<<"Spain">>)
         , coronerl_global:country(<<"Italy">>)
         , coronerl_global:country(<<"France">>, false)
         , coronerl_global:country(<<"Belgium">>)
         , coronerl_global:country(<<"Netherlands">>)
         , coronerl_global:country(<<"Germany">>)
         , coronerl_global:country(<<"Switzerland">>)
         , coronerl_global:country(<<"Austria">>)
         , coronerl_global:country(<<"Slovenia">>)
         , coronerl_global:country(<<"Hungary">>)
         , coronerl_global:country(<<"Slovakia">>)
         , coronerl_global:country(<<"Czechia">>)
         , coronerl_global:country(<<"Poland">>)
         , coronerl_global:country(<<"Korea, South">>)
         , coronerl_global:country(<<"Japan">>)
         , coronerl_global:country(<<"Taiwan*">>)
         , coronerl_global:country(<<"Singapore">>)
         , coronerl_global:country(<<"US">>)
         , coronerl_global:country(<<"Canada">>)
         , coronerl_global:country(<<"Brazil">>)
         , coronerl_global:country(<<"Russia">>)
         %, coronerl_global:country(<<"India">>)
         , coronerl_global:country(<<"China">>)
         , coronerl_global:country(<<"South Africa">>)
         , coronerl_global:country(<<"Australia">>)
         , coronerl_global:country(<<"Iran">>)
         ]
     },
  {continue, Result}.