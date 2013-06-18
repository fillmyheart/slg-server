-module(database_init_201306181371539185).
-export([up/0, down/0]).

up() ->
  io:format("create up ~n"),
  ok.

down() ->
  io:format("create down ~n"),
  ok.
