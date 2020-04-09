-module(coronerl_global_controller).

%% API
-export([ get/2
        , post/2
        ]).

get(_Params, _State) ->
  Result = <<"hello world">>,
  {continue, Result}.

post(_Params, _State) ->
  Result = coronerl_csv_global:reset(),
  {continue, Result}.