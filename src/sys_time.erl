%% 提供时间操作
-module(sys_time).
-export([current/0]).

%% 计算当前时间的秒数时间戳.
current() ->
  {MegaSecs, Secs, _} = erlang:now(),
  Offset = 0,
  MegaSecs * 1000000 + Secs + Offset.

