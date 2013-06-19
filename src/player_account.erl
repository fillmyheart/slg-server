%% 登陆和注册的sql流程必须在一个事务完成.
%% 用户直接使用角色名登陆，如果角色名不存在，服务器则创建一个。
%% 登陆的数据初始化必须在一个事务完成.
%%
-module(player_account).
-export([login_req/1, building_up_req/1, building_upl_req/1, building_del_req/1]).

-include("proto.hrl").
-include_lib("eunit/include/eunit.hrl").

find_create_d(DbDevice=#db_device{udid=Udid}) ->
  case model_devices:select([{udid, Udid}]) of
    [] -> DbDevice1 = DbDevice#db_device{id = data:id(devices)},
          ok = model_devices:insert(DbDevice1),
          DbDevice1;
    [Db] -> Db
  end.

find_create_u(DbDevice, #pt_ubase{name=Name, sex=Sex}) ->
  #db_device{id=Did} = find_create_d(DbDevice),
  case model_users:select(normal, [{name, Name}, {device_id, Did}]) of
    [Result] -> {ok, Result};
    [] -> %% 不存在则新建
      Id = data:id(users),
      Usr = #db_user{name=Name, sex=Sex, id=Id, device_id=Did, user_id=Id},
      case model_users:insert(Usr) of
        ok -> {ok, Usr};
        _ -> error
      end
  end.

login_req(#pt_account{device=Device, base=Base}) ->
  case model:trans(normal, fun()-> find_create_u(Device, Base) end) of
    {atomic, {ok,R}} ->
      erlang:put(u_id, R#db_user.id),
      base_init(R#db_user.id);
    timeout -> player:code_ack(timeout);
    error -> player:code_ack(usr_name_duplicate)
  end,
  ok.

%% 玩家登陆，将主要的数据load到ets.
base_init(UsrId) ->
  {ok, U} = data:lookup_s(users, UsrId),
  {ok, Buildings} = data:lookup_a(buildings, UsrId),
  PT = #pt_snapshot{user=U, buildings=Buildings},
  player:send(snapshot_ack, PT),
  erlang:put(user_id, UsrId),
  ok.

%% 建立建筑请求.
building_up_req(#pt_building{b_type=Type}) ->
  io:format("building ~p~n", [Type]),
  ID = data:id(buildings),
  UID = erlang:get(u_id),
  B = #db_building{id=ID, user_id = UID, level=1, b_type=Type},
  data:add_i(buildings, UID, B),
  ok.

%% 升级建筑请求.
building_upl_req(#pt_building{id=Id}) ->
  io:format("building l ~p~n", [Id]),
  {ok, Db} = data:lookup_i(buildings, Id),
  Level = Db#db_building.level,
  data:update_i(buildings, Db#db_building{level = Level+1}),
  ok.

%% 升级建筑请求.
building_del_req(#pt_building{id=Id}) ->
  io:format("building del ~p~n", [Id]),
  data:delete_i(buildings, erlang:get(user_id), Id),
  ok.
