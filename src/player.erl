%% 处理conn进程的回调函数.
-module(player).
-compile([export_all]).

-include("proto.hrl").

terminate(_Reason, _State) ->
  data:guard_f(),
  ok.

code_ack(Api, Code) ->
  ApiI = proto_api:key(Api),
  CodeInt = proto_error:key(Code),
  Common = #pt_code{code=CodeInt, api=ApiI},
  conn:send(code_ack, Common),
  ok.
