-module(slg_server).
-export([start/0, migrate_do/0, migrate_redo/0, migrate_new/1]).

-include("model.hrl").
-include("proto_record.hrl").
-include("gd_record.hrl").

%% 启动slg-csv配置文件部分.
csv_config() ->
  slg_csv:root("data/"),
  slg_csv:add({gd_vip_exp, [public, duplicate_bag, {keypos,2}]},
              ?csv_record(gd_vip_exp), ["vip_exp.csv"]),
  slg_csv:load(),
  ok.

%% 启动slg-model配置
model_config() ->
  model:init_m(),
  model:sid_s(1),
  Dbc = #db_conf{username=env:username(),
                 password=env:password(),
                 database=env:database()},
  model:add_m(users, record_info(fields, db_user), Dbc),
  model:add_m(devices, record_info(fields, db_device), Dbc),
  model:add_m(buildings, record_info(fields, db_building), Dbc),
  model:add_m(friends, record_info(fields, db_friend), Dbc),
  model:gen_m(),

  %% 生成cache表
  ply_cache:set(users, s),
  ply_cache:set(buildings, a),
  ply_cache:set(friends, a),
  ply_cache:gen(),
  ok.

normal_start() ->
  Dbc = #db_conf{username=env:username(),
                 password=env:password(),
                 database=env:database()},
  model:start(Dbc#db_conf{poll=normal, worker=27}),
  ok.

start() ->
  spt_config:gen(env, "config/server.conf"),
  application:start(log4erl),
  log4erl:conf("config/log4erl.conf"),
  application:start(slg_csv),
  Port = env:listen(),
  slg_proto:start(player, Port),
  application:start(slg_model),
  application:start(slg_server),
  application:start(slg_support),
  csv_config(),
  model_config(),
  normal_start(),
  ply_event:start(),
  ok.

migrate_do() ->
  spt_config:gen(env, "config/server.conf"),
  DataBase = env:database(),
  UserName = env:username(),
  PassWord = env:password(),
  HostName = env:hostname(),
  model_migrate:do("./migrate", HostName, UserName, PassWord, DataBase),
  erlang:halt().

migrate_redo() ->
  spt_config:gen(env, "config/server.conf"),
  DataBase = env:database(),
  UserName = env:username(),
  PassWord = env:password(),
  HostName = env:hostname(),
  model_migrate:redo("./migrate", HostName, UserName, PassWord, DataBase),
  erlang:halt().

migrate_new([List]) ->
  A = list_to_atom(List),
  model_migrate:new("./migrate", A),
  erlang:halt(),
  ok.
