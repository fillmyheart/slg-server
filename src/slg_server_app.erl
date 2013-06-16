-module(slg_server_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


-include_lib("eunit/include/eunit.hrl").

start(_StartType, _StartArgs) ->
  slg_server_sup:start_link().

stop(_State) ->
  ok.

start_test() ->
  ok = application:start(slg_server),
  ?assertNot(undefined == whereis(slg_server_sup)).
