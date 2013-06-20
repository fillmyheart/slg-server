-module(slg_server).
-export([start/0, migrate_do/0, migrate_redo/0]).

-include("model.hrl").
-include("proto_record.hrl").
-include("gd_record.hrl").


%% 启动slg-csv配置文件部分.
csv_config() ->
  slg_csv:root("data/"),
  slg_csv:add({gd_vip_exp, [public, duplicate_bag, {keypos,2}]},?csv_record(gd_vip_exp), ["vip_exp.csv"]),
  slg_csv:load(),
  ok.

%% 启动slg-model配置
model_config() ->
  model:init_m(),
  model:sid_s(1),
  Dbc = #db_conf{username="root", password="", database="slg_server"},
  model:add_m(users, record_info(fields, db_user), Dbc),
  model:add_m(devices, record_info(fields, db_device), Dbc),
  model:add_m(buildings, record_info(fields, db_building), Dbc),
  model:gen_m(), %% 生成配置表
  ok.

normal_start() ->
  model:start(#db_conf{poll=normal, database="slg_server", worker=27}),
  ok.

conn_start() ->
  conn_config:callback(player),
  ok.

start() ->
  application:start(log4erl),
  log4erl:conf("config/log4erl.conf"),
  application:start(slg_csv),
  application:start(slg_proto),
  application:start(slg_model),
  application:start(slg_server),
  application:start(slg_support),
  conn_start(),
  csv_config(),
  model_config(),
  normal_start(),
  ok.

migrate_do() ->
  model_migrate:do("./migrate", "root", "", "slg_server"),
  erlang:halt().

migrate_redo() ->
  model_migrate:redo("./migrate", "root", "", "slg_server"),
  erlang:halt().
