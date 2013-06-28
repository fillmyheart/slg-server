-module(add_friend_id_to_friends_201306231371975270).
-export([up/0, down/0]).

up() ->
  Query = "ALTER TABLE `friends` ADD COLUMN `friend_id` BIGINT(20) NOT NULL  AFTER `user_id` ;
  ",
  model_exec:exec(migrate, Query),
  ok.

down() ->
  io:format("create down ~n"),
  ok.
