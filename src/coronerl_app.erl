%%%-------------------------------------------------------------------
%% @doc coronerl public API
%% @end
%%%-------------------------------------------------------------------

-module(coronerl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  coronerl_http:start(),
  coronerl_csv:init(),
  coronerl_sup:start_link().

stop(_State) ->
  coronerl_http:stop(),
  ok.

%% internal functions