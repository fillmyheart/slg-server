-module(slg_server_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% stop() ->
%%   supervisor:terminate({local, ?MODULE}, ?MODULE).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  %% autoloader 自动加载更新模块
  %% AutoLoader = {
  %%   sys_reloader,
  %%   {sys_reloader, start_link, []},
  %%   permanent,
  %%   infinity,
  %%   worker,
  %%   [sys_reloader]
  %%  },
  %% %% ets表监督进程.
  %% DataSup = {
  %%   data_super,
  %%   {supervisor, start_link, [{local, data_super}, data_super, []]},
  %%   permanent,
  %%   infinity,
  %%   supervisor,
  %%   []
  %%  },
  {ok, {{one_for_one, 5, 10}, []}}.

