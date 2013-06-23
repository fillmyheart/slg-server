%% 由于注册了这些事件，当玩家数据发生了增加，修改和删除时，会自动给客户端发送同步包.
%% ~.~ 不客气

-module(ply_event).
-compile([export_all]).

-include("proto.hrl").

slg_m_upt_s({Table, UsrId, Data}) ->
  case ply_cache1:cache(Table) of
    s -> player:send(spt_atom:atom_suffix(Table, "cah"), Data),
         io:format(">> slg_m_upt_s ~p ~p ~p ~n", [Table, UsrId, Data]);
    _ -> do_nothing
  end.


slg_m_upt_s_e({Table, UsrId, Id, List}) ->
  case ply_cache1:cache(Table) of
    s ->
      io:format(">> slg_m_upt_s_e ~p ~p ~p ~p~n", [Table, UsrId, Id, List]),
      {ok, Db} = data:lookup_s(Table, UsrId),
      player:send(spt_atom:atom_suffix(Table, "cah"), Db);
    _ -> do_nothing
  end.

slg_m_upt_i({Table, Data}) ->
  case ply_cache1:cache(Table) of
    a ->
      io:format(">> slg_m_upt_i ~p ~p~n", [Table, Data]),
      player:send(spt_atom:atom_suffix(Table, "cah_upt"), Data);
    _ -> do_nothing
  end.

%% 没办法，没实现单字段更新包
slg_m_upt_i_e({Table, Id, List}) ->
  case ply_cache1:cache(Table) of
    a ->
      io:format(">> slg_m_upt_i_e ~p ~p ~p~n", [Table, Id, List]),
      {ok, B} = data:lookup_i(Table, Id),
      player:send(spt_atom:atom_suffix(Table, "cah_upt"), B);
    _ -> do_nothing
  end.


%% %% 设每个玩家的s数据不会被删除，如果你删了，确定逻辑无误??
%% slg_m_del_s({Table, UsrId, Id}) ->
%%   ok.

slg_m_del_i({Table, UsrId, Id}) ->
  case ply_cache1:cache(Table) of
    a ->
      io:format(">> slg_m_del_i ~p ~p ~p~n", [Table, UsrId, Id]),
      player:send(spt_atom:atom_suffix(Table, "cah_del"), #pt_pkid{id=Id});
    _ -> do_nothing
  end.

slg_m_del_i_a({Table, UsrId, Ids}) ->
  case ply_cache1:cache(Table) of
    a ->
      io:format(">> slg_m_del_i_a ~p ~p ~p~n", [Table, UsrId, Ids]),
      player:send(spt_atom:atom_suffix(Table, "cah_dels"), #pt_pkids{ids=Ids});
    _ -> do_nothing
  end.

%% %% s数据加入不发生更新包.
%% slg_m_add_s({Table, UsrId, Data}) ->
%%   ok.

slg_m_add_i({Table, UsrId, Data}) ->
  case ply_cache1:cache(Table) of
    a ->
      io:format(">>slg_m_add_i  ~p ~p ~p~n", [Table, UsrId, Data]),
      player:send(spt_atom:atom_suffix(Table, "cah_new"), Data);
    _ -> do_nothing
  end.


start() ->
  spt_notify:sub(slg_m_upt_s, fun ply_event:slg_m_upt_s/1),
  spt_notify:sub(slg_m_upt_s_e, fun ply_event:slg_m_upt_s_e/1),
  spt_notify:sub(slg_m_upt_i, fun ply_event:slg_m_upt_i/1),
  spt_notify:sub(slg_m_upt_i_e, fun ply_event:slg_m_upt_i_e/1),
  %% spt_notify:sub(slg_m_del_s, fun ply_event:slg_m_del_s/1),
  spt_notify:sub(slg_m_del_i, fun ply_event:slg_m_del_i/1),
  spt_notify:sub(slg_m_del_i_a, fun ply_event:slg_m_del_i_a/1),
  %% spt_notify:sub(slg_m_add_s, fun ply_event:slg_m_add_s/1),
  spt_notify:sub(slg_m_add_i, fun ply_event:slg_m_add_i/1),
  ok.
