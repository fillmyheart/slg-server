-module(slg_server).
-export([start/0]).

-include("model.hrl").
-include("proto_record.hrl").

-define(csv_record(Name), {Name, record_info(fields, Name)}).

%% vip经验配置,默认为0级.
-record(gd_vip_exp, {
          level,                    %% vip升级等级
          exp,                      %% 升级到level需要的经验.
          array
         }).

%% 启动slg-csv配置文件部分.
csv_config() ->
  slg_csv:root("data/"),
  slg_csv:add({gd_vip_exp, [public, duplicate_bag, {keypos,2}]},
              ?csv_record(gd_vip_exp),
              %%[{fun csv_inject2:inject_vip_exp/2, "vip_exp.csv", 2}]
              ["vip_exp.csv"]),
  slg_csv:load(),
  ok.

%% 启动slg-model配置
model_config() ->
  model:init_m(),
  model:add_m(users, record_info(fields, db_user), "slg_server"),
  model:add_m(devices, record_info(fields, db_device), "slg_server"),
  model:add_m(buildings, record_info(fields, db_building), "slg_server"),
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
