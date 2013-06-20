# 0. 获取代码
---

首先建立你自己的git仓库，然后执行`git pull origin git@42.121.105.8:slg-server.git master` 获取最新的代码。

如果`slg-server`有改变或者bug修改，同样执行一次。

# 1. 初始化
---

*获取代码:* `git clone git@42.121.105.8:slg-server.git`

*获取依赖关系:* `make deps`

*生成协议代码:* `make g`

*编译：* `make`

*初始化数据库:* `make db`

*启动:* `make s`，默认用户名加密码为：root，密码空；如果不是，自行修改slg_server.erl

*测试:* 在新的终端执行`make e`，执行: `gt:start()`.

# 2. 逻辑模块接口和规范
---

## 2.1 简介
本项目是为小服设计的slg游戏服务器框架，主要包含以下四个组件:

* slg_proto:处理网络连接，数据的序列化，加密解密处理.
* slg_csv:游戏中常常有gd配置文件，slg_csv提供配置文件到ets表的直接映射，便于编程访问.
* slg_model:本框架使用ets在内存中cache玩家数据，slg_model提供ets和mysql的表映射，提供cache和自动写回等功能。
* slg_support:提供一些系统和逻辑工具，比如spt_reloader：提供热更新,spt_notify：提供notification事件模式，等等。

(这四个组件也可以用于组合成分布式的单服).

每一个组件都对应于单独的项目，你可以从`config/rebar.config`看出，下面会对每一个模块的使用方法进行详细描述。

## 2.2 连接协议处理：slg_proto

slg_proto设计为处理游戏服务器中的tcp连接和协议打包解包，加密解密，玩家的基本进程运行在此模块.

### 2.2.1 协议规范

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

### 2.1.2 回调处理

`player.erl`模块有以下几个回调函数:

`quit/2:` 进程退出，可以视为玩家退出登陆的事件点。
`cast/2` 处理异步消息，gen_server:cast即可。
`info/2` 处理info，gen_server。
`call/3` 处理call，gen_server。

## 2.3 gd配置文件处理

### 2.3.1 slg-csv启动
游戏中有大量的gd配置文件，gd采用ms-excel进行编辑后，服务器开发人员使用程序data/data.py脚本将其导出为csv文件，而slg_csv模块提供csv文件到ets表的直接映射。

slg_csv模块的使用大概如下:

`slg_csv:root("data/")` 指定了游戏中csv文件存放的路径.
`slg_csv:add` 加入一个新的映射项。
`slg_csv:load()` 执行csv配置文件加载.

以上代码的执行必须在`application:start(slg_csv),`，在`slg-server`的start函数中已经集成了。

在头文件`include/gd_record.hrl`存放所有gd配置的映射数据结构.

在`data/`路径下存放所有的csv文件。

本项目在slg_server:csv_config对slg-csv模块进行的初始化，你可以把它移动到任意一个你想它去的地方。

### 2.3.2 数据类型

本模块要求csv文件中的第一行(列名)必须和其对应的gd配置文件的record一致，比如：

`gd_record.hrl`: -record(gd_vip_exp, {level, exp}).
`data/xx.csv`: INT_level;INT_exp

record里面不需要指定字段的类型，因为erlang是无类型的，但是csv文件里的第一行需要字典各列的类型，来帮助映射.

类型名与字段名通过下划线分割，比如：`INT_level;INT_exp`

在csv文件中不字段不区分大小写，现在支持以下类型：

* int:数字，也是默认类型，也就是不制定类型则为int
* str:字符串
* term:erlang Term.

### 2.3.3 例子

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

## 2.4 slg-model玩家数据处理.

程序使用ets表缓存玩家数据，查找数据时会先在ets表中进行，如果不存在则会在MySql中查询，同样，一些删除，更新，插入操作也在ets中直接执行，slg-model会实时的发送给异步持久化进程。

### 2.4.1 初始化方法

在`slg_server.erl`的model_config函数对model层进行了初始化:

    model:init_m(),
    Dbc = #db_conf{username="root", password="", database="slg_server"},
    model:add_m(users, record_info(fields, db_user), Dbc),
    model:add_m(devices, record_info(fields, db_device), Dbc),
    model:add_m(buildings, record_info(fields, db_building), Dbc),
    model:gen_m(), %% 生成配置表

其中`db_user`和`db_device`都是在协议中定义的数据结构，而这个协议结构的字段和其类型必须和其对应的mysql表一一对应。

model:_add_m(表名，数据类型，连接配置).


### 2.4.2 数据操作

数据操作函数集中在`data.erl`模块，如下：

    -export([lookup_s/2, lookup_a/2, lookup_i/2]).
    -export([update_s/3, update_i/2, delete_i/3, delete_s/3, clear/1]).
    -export([add_s/3, add_i/3, id/1]).

分别对应与增删查改。

约定：

*_s：* 为后缀的适合每个玩家只有一条的数据，比如玩家数据。
*_a：* 为后缀的适合每个玩家有多条的数据，比如建筑。

在player_account里有基本的例子。

### 2.4.3 按元素查找和更新.

`data.erl`模块中有`lookup_s_e， lookup_i_e, update_i_e, update_s_e`等四个函数，他们可以在模块被初始化之后使用，参看`player_account:building_upl_req`

## 2.5 slg-support 公共模块

本组件提供一些erlang编程或系统工具集合。

* spt_reloader-热更新
* spt_notify-事件注册/分发
* spt_smerl-动态模块编程库

### 2.5.1 spt_reloader-热更新

热更新进程，来源于michi-web，可以对线上运行的代码进行热替换，但应该遵守以下面规则：

* 新代码必须没有改变上下文数据结构，才能进行热部署.
* 有一个以上模块被修改，模块被reload的顺序不可保证，因此，如果你的函数原型发生了变化，或者调用了新增的函数，热部署都很危险。
* 对在单个模块代码bug的hotfix，非常适合直接reloader。

默认情况下spt_reloader启动，当beam代码发生变化时将会自动热更新。

### 2.5.2 spt_notify-事件注册/分发.

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


### 2.5.3 spt_smerl-动态模块编程库

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


# 3 slg-proto连接处理和协议设计
---

slg-proto提供了连接处理和协议打包，加密三个功能;

## 3.1 连接处理

因为erlang使用了轻量级进程，所以连接处理代码比较简单，主要分为以下三个部分:

* conn_acceptor:监听某端口，在连接建立起时使用conn.erl开启一个新进程，并设置这个进程和新建的socket关联。
* conn.erl:连接通信实体进程，通过handle_info({tcp, _Socket, Bin}, State)匹配处理socket发来的数据。
* conn_super.erl：conn进程的监督者，用来启动conn而已。

本项目使用tcp长连接，主要从[hotwheels](git@github.com:tolbrino/hotwheels.git)移植过来，`hotwheels`是erlang中tcp连接处理的典范。

如果你使用过其他语言如C/C++，连接处理就不需要自己实现epoll或select之类的io多路复用，这些在erlang底层都帮你处理了，所以使用erlang编写这部分代码非常简单易懂。

有了传输层代码，下面介绍协议层，即传输数据包设计.

## 3.2 包设计

每一个数据包都由三部分组成： `包长度(2字节)+包类型(2字节)+包数据(剩余字节)`。

* 每个正常包的前两个字节都是包长，也就是你必须先收两个字节，确定之后的包的长度，等待完整包收完后再处理。
* 包类型为2个字节，最多有65535个不同的包，完全够用了。
* 包的剩余部分是包内容，是被序列化为2进制的数据结构，有很多同类型的产品(google protobuff ,apache thrift)，但是slg_proto足够简单，足够适合游戏。

erlang语言本身在建立socket的时候可以指定参数:

    inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),

### 3.2.1 包长度

其中`{packet, 2}`制定后，对于所有的gen_tcp:send操作，erlang将自动在send的内容之间加上2个字节的内容长度，也就是协议中包长度部分。

### 3.2.2 包类型

一个完整的包收好后，将通过包类型来决定如何处理，包类型的描述我定义在了`proto/api.txt`文件中，每个包有以下5个属性需要配置:

* packet_type:包类型，必须是一个唯一的正数，10000以上为系统功能错误码，10000以下为逻辑错误码
* name:包名字，毕竟数字在代码里不是很友好
* payload:包内容，这说明包类型确定后，包内容也确定了
* desc:一句注释而已.
* module:请求处理模块，只对以_req为后缀的包名字的包有用，可以直接映射到处理模块。

### 3.2.3 包内容

数据包的内容我称其为payload，他们全部在`proto/protocal.txt`中被定义，包内容定义比较复杂，以下是基础类型，你可以通过基础类型组合成自定义类型：

* integer：数字类型
* float:浮点类型
* string:字符串类型，实际上处理为erlang的binary类型
* boolean：布尔类型，只占1个字节
* short：端整型
* pkid：主键类型，大整数，erlang层是数字，因为erlang支持大数，但是序列化为字符串.

有了基本类型，你可以自定义一个用户类型：

    pt_user=
    name string
    sex boolean
    ===

非常简单，你可以定义一个账号类型，它嵌套了用户类型:

    pt_account=
    user pt_user
    money integer
    ===

很多情况下我们需要数组，你可以这样定义一个含有数组的类型：

    pt_test=
    ids array integer
    users array pt_user
    ===

## 3.3 错误处理

游戏中有很多错误码，什么金钱不足，等级不足之类的，建议在`proto/error_code.txt`中列出你所有的错误码，大概如下：

    10000-ok-成功
    10001-inner_error-内部错误
    10002-bad_param-参数错误

在api.txt中定义了code_ack作为统一的错误接口，90%的错误提示可以使用该包完成，在代码中你使用起来像这样：

    conn:code_ack(timeout)

# 4 slg-model数据存储设计
---

slg_model旨在提供这样一个模块：

游戏服务器中，用户登陆的时候会把玩家的数据从数据库总load到内存中，之后就在内存中对玩家数据进行操作，定时写回到数据库进行持久化（这里我选用了MySql数据，在游戏行业用的比mongodb稳定广泛），而这一套机制，可以作为服务框架的一部分存在，不需要框架的使用者编程，只需要他们按照相应的接口编写逻辑即可。

slg_model提供了以下三个重要的功能：

* SQL操作：对底层的直接SQL执行封装了比较友好的上层操作，类似于active_record，但是没有那么强大，也没有表表关联，但是游戏服务器也不需要表表关联或者复杂的查询。
* ets缓存：将用户数的数据暂存在ets表中，通过一定的机制将不活跃的内存清除掉，用户在使用时不需要知道ets和MySql的存在，比如查找数据，系统先会在ets表中查找，如果不存在则cache不命中，再从MySql中查找。
* 自动回写和同步：在内存中插入或者修改删除了数据，系统会自动将其同步到MySql，同步时间间隔大概为1分钟，对于每个表，都开了两个erlang-mysql-driver连接池，一个读(3worker)，一个写(1worker).
* migrate：在游戏开发的过程中，常常会对表结构进行修改，比较土的方法是删除重建整个数据库，而使用migrate对表格进行修改会比较方便，完全仿照Ruby On Rails的migrate做，但是不强大，migrate函数中只能执行SQL，没有封装上层操作。

## 4.1 ets表

### 4.1.1 为什么不使用mnesia

mnesia是erlang提供的分布式数据库，功能比较强大，但它的功能对于我设计游戏服务器并不是有用，暂时没有需要使用mnesia的理由，如果设计单服的游戏服务器，并且要防止单点故障，那么可能用mnesia比较合适。

再次说明本模块主要为小服的游戏服务器设计的。

### 4.1.2 为什么不使用进程字典

国内有些页游开发团队使用进程字典存储玩家数据，即玩家登陆后将其数据加载到进程字典，然后就在进程字典中操作了，并且按时写回MySql，这样主要的好处是：进程字典操作比较快，纳秒级别；ets操作比较慢，微秒级别。

主要的坏处是：

* 必须使用防御式编程：进程字典数据是随着进程的退出而消失的，所以你的进程将不能再动不动就crash了，于是你会编写大量的防御式编程代码，将erlang本身优雅的函数式编写的奇丑无比；而使用ets，玩家进程不在了ets表数据也不会丢失，可以随便崩。
* 完全写不了单元测试：理论上函数式编程是最好写单元测试，但是你现在每个函数都跟进程上下文相关了，如果要写的话每个单元测试函数你都需要建立起一个新进程，我感觉是没法写，丑的很。

ets也不是慢，微秒级别，对于小服，拖个几千人就ok了，完全够用，所以我采用ets，也推荐你采用。 

*进程字典只适合存储临时数据*，那种丢了就丢了，或者session相关的，不需要造成你防御式编程的，其它场景`慎用`。

### 4.1.3 以表组织数据的好处

按表组织玩家数据有以下好处：

*`灵活访问非在线玩家数据`* 当采用进程字典存储玩家数据时，任何对其它玩家数据的访问都需要通过进程通信获得数据，比如好友系统，只需要获取这个好友的名字和账号信息，这时需要建立起这个玩家的进程，然后走消息通信来获得数据，而使用ets表组织，你只需要拿user_id索引其基本信息表即可，非常优雅。

*`手机游戏的异步性`* 手机游戏对非在线玩家的数据访问非常频发，因为玩手机游戏的的时间是零碎的，需要灵活的操作非在线玩家数据。


## 4.2 实时持久化

在ge2里，数据大概是每10触发一次同步，但是在slg_server里采用了实时同步，这样的好处是玩家数据更稳定，更不容易丢失，坏处是MySql压力更大，但是slg_server面对的是小服，一个服务器人数是非常有限的(几W)，同时在线人数(6K)，负载压力本身不会很大，而且持久化过程是异步的，不影响玩家体验。

设计以下几个模块：

*`model_sql.erl:`* 提供SQL拼接功能，如果更复杂的SQL操作应丰富这个模块。
*`model_exec.erl:`* 执行SQL，使用erlang-mysql-driver实现。
*`model.erl:`* 提供model操作的统一接口

对每一张表都有一个单独的进程来进行会写，`data_writer_sup.erl`和`data_writer`。

## 4.3 清除不活跃数据

slg_model中使用data_clear清除不活跃数据，使用时间超过6小时，并且当前没有人使用的数据被定义为不活跃数据，此部分数据将以玩家为单位执行清除。

每个表有单独的清除进程，代码集中在：`data_clear_sup.erl`和`data_clear.erl`

### 4.3.1 ets的原子性

`ets`表不是`数据库`，是对其基本操作加了读写锁的内存表，所以下面的代码会有很大问题：

	R = read_ets(Id)          # 1
	if
		R == ok ->
			set_ets(Id, no)   # 2
			do_something      # 3
		no -> do_nothing
	end

在`#1`和`#2`之间没有数据库的隔离性，所以同时会有多个进程可能进入代码`#3`，引发逻辑错误，比如清除玩家数据就是这样一个场景。

`data_clear`会选择不活跃且没人使用的玩家数据来清除，这其中会破坏ets表的结构，而如果同时又有用户登陆并对其进行操作，就会有逻辑错误。

### 4.3.2 `data_guard`

`data_guard.erl`用来解决ets表的数据访问冲突，原理是模拟读写锁，当有玩家使用时获取读锁，而清除进程获取写锁。写锁必须要没有读数或写锁的情况下才能获取成功，读数必须要没有写锁的情况下获取成功。

* 玩家登陆时将获取其数据的读guard，直至进程退出后释放。
* 当进程访问其他玩家数据，也会获取读数，直至进程退出后释放。
* clear进程必须要获取到写锁才能清除数据。

## 4.4 SQL拼接

见代码模块`model_sql.erl`
