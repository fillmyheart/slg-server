%% 登陆和注册的sql流程必须在一个事务完成.
%% 用户直接使用角色名登陆，如果角色名不存在，服务器则创建一个。
%% 登陆的数据初始化必须在一个事务完成.
%%
-module(ply_account).
-export([login_req/1, building_up_req/1, building_upl_req/1, building_del_req/1,
         user_l_req/1, friend_a_req/1, friend_d_req/1, friend_l_d_req/1]).

-include("proto.hrl").
-include_lib("eunit/include/eunit.hrl").

find_create_d(DbDevice=#db_device{udid=Udid}) ->
  case model_devices:select_t([{udid, Udid}]) of
    [] -> DbDevice1 = DbDevice#db_device{id = data:id(devices)},
          ok = model_devices:insert_t(DbDevice1),
          DbDevice1;
    [Db] -> Db
  end.

find_create_u(DbDevice, #pt_ubase{name=Name, sex=Sex}) ->
  #db_device{id=Did} = find_create_d(DbDevice),
  case model_users:select_t([{name, Name}, {device_id, Did}]) of
    [Result] -> {ok, Result};
    [] -> %% 不存在则新建
      Id = data:id(users),
      Usr = #db_user{name=Name, sex=Sex, id=Id, device_id=Did, user_id=Id},
      case model_users:insert_t(Usr) of
        ok -> {ok, Usr};
        _ -> error
      end
  end.

login_req(#pt_account{device=Device, base=Base}) ->
  case model:trans(normal, fun()-> find_create_u(Device, Base) end) of
    {atomic, {ok, R}} ->
      erlang:put(u_id, R#db_user.id),
      base_init(R#db_user.id);
    timeout -> player:code_ack(timeout);
    error -> player:code_ack(usr_name_duplicate)
  end,
  ok.

%% 玩家登陆，将主要的数据load到ets.
base_init(UsrId) ->
  ply_cache:cache_l(UsrId),
  player:send(login_ack, #pt_int{}),
  player:join(UsrId),
  ok.

%% 建立建筑请求.
building_up_req(#pt_building{b_type=Type}) ->
  ID = data:id(buildings),
  UID = erlang:get(u_id),
  B = #db_building{id=ID, user_id = UID, level=1, b_type=Type},
  data:add_i(buildings, UID, B),
  ok.

%% 升级建筑请求.
building_upl_req(#pt_building{id=Id}) ->
  UID = erlang:get(u_id),
  {ok, Level} = data:lookup_i_e(buildings, UID, Id, #db_building.level),
  data:update_i_e(buildings, UID, Id, [{#db_building.level, Level+1}]),
  ok.

%% 升级建筑请求.
building_del_req(#pt_building{id=Id}) ->
  UID = erlang:get(u_id),
  ok = data:delete_i(buildings, UID, Id),
  ok.

%% 请求升级
user_l_req(#pt_int{i=L1}) ->
  UID = erlang:get(u_id),
  {ok, L} = data:lookup_s_e(users, UID, #db_user.level),
  data:update_s_e(users, UID, [{#db_user.level, L+L1}]),
  ok.

friend_a_req(#pt_pkid{id=FriendId}) ->
  UID = erlang:get(u_id),
  {ok, Friends} = data:lookup_a_e(friends, UID, #db_friend.friend_id),
  case lists:member(FriendId, Friends) of
    true -> do_nothing;
    false ->
      ID = data:id(friends),
      B = #db_friend{id=ID, user_id=UID, friend_id=FriendId},
      data:add_i(friends, UID, B)
  end,
  player:code_ack(friend_a_req, ok).

friend_d_req(#pt_pkid{id=Id}) ->
  UID = erlang:get(u_id),
  ok = data:delete_i(friends, UID, Id),
  ok.

%% 一次请求删除多条数据.
friend_l_d_req(#pt_pkids{ids=Ids}) ->
  UID = erlang:get(u_id),
  ok = data:delete_i_a(friends, UID, Ids),
  ok.
