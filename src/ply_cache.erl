%% 指定哪些模块需要被客户端cache.
-module(ply_cache).
-export([set/2, gen/0, cache/2, cache_l/1]).

set(Table, Type) ->
  spt_ets:safe_create(ply_cache_m, [named_table, public]),
  ets:insert(ply_cache_m, {Table, Type}).

gen() ->
  {F, L} = lists:foldl(fun({Table, Type}, {F, L}) ->
                           Fun = io_lib:format("cache(~p) -> ~p;", [Table, Type]),
                           F1 = F ++ lists:flatten(Fun),
                           {F1, [Table|L]}
                       end,
                       {"", [] }, ets:tab2list(ply_cache_m)),
  F1 = F ++ "cache(_) -> no.",
  M1 = spt_smerl:new(ply_cache1),
  {ok, M2} = spt_smerl:add_func(M1, F1),
  F2 = io_lib:format("list() -> ~p.", [L]),
  F3 = lists:flatten(F2),
  {ok, M3} = spt_smerl:add_func(M2, F3),
  %%{ok, M3} = spt_smerl:set_exports(M2, [{cache, 1}]),
  spt_smerl:compile(M3).

cache(Table, UsrId) ->
  {Api, Cache} =
    case ply_cache1:cache(Table) of
      s ->
        {ok, U} = data:lookup_s(Table, UsrId),
        {spt_atom:atom_suffix(Table, "cah"), U};
      a ->
        {ok, Buildings} = data:lookup_a(Table, UsrId),
        {spt_atom:atom_suffix(Table, "cah"),
         {spt_atom:atom_prefix(Table, "db"), Buildings}}
    end,
  player:send(Api, Cache).


%% 登陆时调用此函数，将发送全部需要cache的包，你也可以用上面那函数一个个的发
cache_l(UsrId) ->
  [cache(T, UsrId) || T <- ply_cache1:list()].
