%% 玩家数据事件.
-module(pld_event).
-compile([export_all]).

atom_new(Table) ->
  L = atom_to_list(Table) ++ "_cah_new",
  list_to_atom(L).

slg_m_add_i({Table, _UsrId, Data}) ->
  io:format("=>>>> ev ~p ~p ~p ~n", [Table, _UsrId, Data]),
  player:send(atom_new(Table), Data),
  ok.

start() ->
  spt_notify:sub(slg_m_add_i, fun pld_event:slg_m_add_i/1).
