%% 处理conn进程的回调函数.
-module(player_state).
-compile([export_all]).

terminate(_Reason, _State) ->
  data:guard_f(),
  ok.
