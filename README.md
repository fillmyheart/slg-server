# 1. 初始化
---
*获取代码:* `git clone git@42.121.105.8:slg-server.git`

*获取依赖关系:* `make deps`

*生成协议代码:* `make g`

*编译：* `make`

*初始化数据库:* `make db`

*启动:* `make s`，默认用户名加密码为：root，密码空；如果不是，自行修改slg_server.erl

*测试:* 在新的终端执行`make e`，执行: `gt:start()`.

# 2. 逻辑模块介绍
---
本项目是为小服设计的slg游戏服务器框架，主要包含以下四个组件:

* slg_proto:处理网络连接，数据的序列化，加密解密处理.
* slg_csv:游戏中常常有gd配置文件，slg_csv提供配置文件到ets表的直接映射，便于编程访问.
* slg_model:本框架使用ets在内存中cache玩家数据，slg_model提供ets和mysql的表映射，提供cache和自动写回等功能。
* slg_support:提供一些系统和逻辑工具，比如spt_reloader：提供热更新,spt_notify：提供notification事件模式，等等。

(这四个组件也可以用于组合成分布式的单服).

每一个组件都对应于单独的项目，你可以从`config/rebar.config`看出，下面会对每一个模块的使用方法进行详细描述。

# 3 连接协议处理：slg_proto
---
slg_proto设计为处理游戏服务器中的tcp连接和协议打包解包，加密解密，玩家的基本进程运行在此模块.

## 3.1 协议规范

在目录proto/下定义项目需要的包和协议，需要遵守以下规范：

`api.txt` 定义了服务器中所有的包类型。

* 每个协议的包名需要以:`_req，_ack，_ntf，_cah`结尾，如果违反`make g`时会报错.
* 每个req包必须要有一对应的回复ack包，这两种包用作同步请求处理。
* ntf包用作服务器主动向客户端push的包，比如聊天和一些消息通知，用作客户端临时使用。
* cah包用作客户端cache，每一个cah包都会被客户端缓存，里面保存了特定模块的数据，比如：building_cah保存建筑数据，客户端会直接视cache包为其内部数据结构使用，而当某个模块数据发生变化时，服务器可以直接推送cache包，客户端直接覆盖更新.

`proto.txt` 定义了所有的包内容.

包内容的定义需要按找protocal.txt里的描述使用，有几点注意：

* 普通类型全部以pt_为前缀。
* 数据结构以db_为前缀，意为此db_数据结构与数据库中结构相对应，它的字段名字和类型都必须与服务器数据库中表定义一致，否则出错。
* 每一个db_内容包的第一个字段都必须是id，给与其类型为pkid。

例子：登陆流程

玩家登陆时，客户端发送登陆_req，服务器验证成功后，先发送多个cache包，让客户端cache部分数据，之后发送登陆成功_ack。

`error_code.txt` 包含了游戏中可能出现的错误码，需要以下规范：

* 10000以上为系统功能错误。
* 10000以下为逻辑功能错误.
* 对于一些简单错误，用`player:code_ack`回复。
* 使用`player:send`给客户端发送各种包。

修改proto后执行`make g && make`可生成可编译新的协议处理版本。

## 3.2 回调处理

`player.erl`模块有以下几个回调函数:

`quit/2:` 进程退出，可以视为玩家退出登陆的事件点。
`cast/2` 处理异步消息，gen_server:cast即可。
`info/2` 处理info，gen_server。
`call/3` 处理call，gen_server。

# 4. gd配置文件处理
---

## 4.1 slg-csv启动
游戏中有大量的gd配置文件，gd采用ms-excel进行编辑后，服务器开发人员使用程序data/data.py脚本将其导出为csv文件，而slg_csv模块提供csv文件到ets表的直接映射。

slg_csv模块的使用大概如下:

`slg_csv:root("data/")` 指定了游戏中csv文件存放的路径.
`slg_csv:add` 加入一个新的映射项。
`slg_csv:load()` 执行csv配置文件加载.

以上代码的执行必须在`application:start(slg_csv),`，在`slg-server`的start函数中已经集成了。

在头文件`include/gd_record.hrl`存放所有gd配置的映射数据结构.

在`data/`路径下存放所有的csv文件。

本项目在slg_server:csv_config对slg-csv模块进行的初始化，你可以把它移动到任意一个你想它去的地方。

## 4.2 数据类型

本模块要求csv文件中的第一行(列名)必须和其对应的gd配置文件的record一致，比如：

`gd_record.hrl`: -record(gd_vip_exp, {level, exp}).
`data/xx.csv`: INT_level;INT_exp

record里面不需要指定字段的类型，因为erlang是无类型的，但是csv文件里的第一行需要字典各列的类型，来帮助映射.

类型名与字段名通过下划线分割，比如：`INT_level;INT_exp`

在csv文件中不字段不区分大小写，现在支持以下类型：

* int:数字，也是默认类型，也就是不制定类型则为int
* str:字符串
* term:erlang Term.

## 4.3 例子

`slg_csv:add`接受3个参数来添加映射：(ets表设定，record指定，文件指定):

例如：
    slg_csv:add({gd_vip_exp, [public, duplicate_bag, {keypos,2}]},?csv_record(gd_vip_exp), ["vip_exp.csv"])

`{gd_vip_exp, [public, duplicate_bag, {keypos,2}]}`:ets表名为：gd_vip_exp，属性:[public, duplicate_bag, {keypos,2}]；也可以只是一个`gd_vip_exp`。
`?csv_record(gd_vip_exp)`其映射的record为gd_vip_exp。
`["vip_exp.csv"]`对应的csv文件.

`ets属性指定`

默认ets属性为`[public, set, named_table, {keypos, 2}]`， 你也可以自己制定类型属性，比如.

    %% 指定`gd_vip_exp`为bag类型
    add({gd_vip_exp, [bag]}, ?csv_record(gd_vip_exp), ["vip_exp.csv"]),

也就是每个ets表可以通过 `atom | {atm, [attr|attr]}` 指定。

`多ets表指定`

可以在add的第一个参数指定一个列表，已达到创建多个ets表的目的，但只有第一个ets表为csv文件的导入表，其余的用户自定义为辅助表或结果表.

    add([gd_vip_exp, defined_but_not_use] , ?csv_record(gd_vip_exp), ["vip_exp.csv"]),

`多csv文件`

定义多csv文件将把多个csv文件导入一个相同的ets表:

    add(gd_vip_exp, ?csv_record(gd_vip_exp), ["vip_exp.csv", "vip_exp2.csv"]),

注意重复的id会给出错误提示，你可以选择忽略。

`inject函数`

大部分时候csv表到ets是可以直接映射过去，但是不有的时候也需要手工处理一下再导入到ets，这时候可以使用inject函数.

inject函数是一个普通函数，它接受一个record为参数，返回另一个处理之后的record，只有slg_csv会把这个record导入到ets.

比如，你可以把你的inject函数定义到一个单独的erlang模块:

    -module(csv_inject).
    -export([compile_all]).

    inject_vip_exp(Vip) ->
        io:format("vip ~p~n", [Vip]).

然后通过以下参数指定:

    add(gd_vip_exp, ?csv_record(gd_vip_exp), [ {fun csv_inject/inject_vip_exp/1, "vip_exp.csv"}]),

inject函数还可以定义一个参数，但是函数原型为:

    inject_vip_exp(Vip, P) ->
        io:format("vip ~p ~p~n", [Vip, P]),
        Vip.

    add(gd_vip_exp, ?csv_record(gd_vip_exp), [ {fun csv_inject/inject_vip_exp/2, "vip_exp.csv", 23}]),

可以slg_csv下代码:csv_inject2.erl
ok.

# 5 slg-model玩家数据处理.
---

程序使用ets表缓存玩家数据，查找数据时会先在ets表中进行，如果不存在则会在MySql中查询，同样，一些删除，更新，插入操作也在ets中直接执行，slg-model会实时的发送给异步持久化进程。

## 5.1 初始化方法

在`slg_server.erl`的model_config函数对model层进行了初始化:

    model:init_m(),
    Dbc = #db_conf{username="root", password="", database="slg_server"},
    model:add_m(users, record_info(fields, db_user), Dbc),
    model:add_m(devices, record_info(fields, db_device), Dbc),
    model:add_m(buildings, record_info(fields, db_building), Dbc),
    model:gen_m(), %% 生成配置表

其中`db_user`和`db_device`都是在协议中定义的数据结构，而这个协议结构的字段和其类型必须和其对应的mysql表一一对应。

model:_add_m(表名，数据类型，连接配置).


## 5.2 数据操作

数据操作函数集中在`data.erl`模块，如下：

    -export([lookup_s/2, lookup_a/2, lookup_i/2]).
    -export([update_s/3, update_i/2, delete_i/3, delete_s/3, clear/1]).
    -export([add_s/3, add_i/3, id/1]).

分别对应与增删查改。

约定：

*_s：* 为后缀的适合每个玩家只有一条的数据，比如玩家数据。
*_a：* 为后缀的适合每个玩家有多条的数据，比如建筑。

在player_account里有基本的例子。

# 6. slg-support 公共模块
---

本组件提供一些erlang编程或系统工具集合。

* spt_reloader-热更新
* spt_notify-事件注册/分发
* spt_smerl-动态模块编程库

## 6.1.spt_reloader-热更新

热更新进程，来源于michi-web，可以对线上运行的代码进行热替换，但应该遵守以下面规则：

* 新代码必须没有改变上下文数据结构，才能进行热部署.
* 有一个以上模块被修改，模块被reload的顺序不可保证，因此，如果你的函数原型发生了变化，或者调用了新增的函数，热部署都很危险。
* 对在单个模块代码bug的hotfix，非常适合直接reloader。

默认情况下spt_reloader启动，当beam代码发生变化时将会自动热更新。

## 6.2.spt_notify-事件注册/分发.

游戏中需要关注很多事件的发生，比如建筑升级事件，玩家打赢了一个boss事件，spt_notify提供事件的注册和发生接口，有以下3个api:

* sub(Event, Fun):订阅
* unSub(Event, Fun)：取消订阅
* post(Event, Param):事件发生.

post事件的第2个参数将会被原样传递给注册的函数，使用例子如下：

    Fun1 = fun(X) ->  io:format("x1 ~p~n", [X]) end,
    Fun2 = fun(X) ->  io:format("x2 ~p~n", [X]) end,
    spt_notify:sub(e1,  Fun1),
    spt_notify:sub(e1, Fun2),
    spt_notify:post(e1, 23),
    spt_notify:ubsub(e1, Fun1),
    spt_notify:post(e1, 23),


## 6.3.spt_smerl-动态模块编程库

虽然erlang的动态编程能力不强(也或者是我学的很浅)，但是smerl这个模块用来做动态模块扩展是比较合适的，它来源于erlyweb项目，已经稳定了很多年。

以下情况适合使用smerl动态产生模块:

* 大量的重复编程模块：比如slg_model里的表model，基本结构都一样(select, update, delete)，只有一点参数的不同而已。
* 环境参数：有的环境参数，如果放在ets表又太慢了，放在固定的模块在每次修改时又需要编译，所以我倾向于动态产生一个模块，然后从模块函数里直接获取配置参数。

使用方法如下，来源于源码注释：

    test_smerl() ->
    M1 = spt_smerl:new(foo),
    {ok, M2} = spt_smerl:add_func(M1, "bar() -> 1 + 1."),
    spt_smerl:compile(M2),
    foo:bar(),   % returns 2``
    spt_smerl:has_func(M2, bar, 0). % returns true

