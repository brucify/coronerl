-module(coronerl_http).

%% API
-export([ start/0
        , stop/0
        ]).

-define(HTTP_LISTENER, my_http_listener).

start() ->
  {ok, Routes} = file:consult(filename:join(code:priv_dir("coronerl"),"routes.src")),

  Dispatch = cowboy_router:compile([
    {'_', Routes}
  ]),

  {ok, _} = cowboy:start_clear(?HTTP_LISTENER,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
  ).

stop() ->
  cowboy:stop_listener(?HTTP_LISTENER).