# 初始化
---
*获取代码:* `git clone git@42.121.105.8:slg-server.git`

*获取依赖关系:* `make deps`

*生成协议代码:* `make g`

*编译：* `make`

*初始化数据库:* `make db`

*启动:* `make s`，默认用户名加密码为：root，密码空；如果不是，自行修改slg_server.erl

*测试:* 在新的终端执行`make e`，执行: `gt:start()`.

# 逻辑模块
---
本项目是为小服设计的slg游戏服务器框架，主要包含以下四个组件:

* slg_proto:处理网络连接，数据的序列化，加密解密处理.
* slg_csv:游戏中常常有gd配置文件，slg_csv提供配置文件到ets表的直接映射，便于编程访问.
* slg_model:本框架使用ets在内存中cache玩家数据，slg_model提供ets和mysql的表映射，提供cache和自动写回等功能。
* slg_support:提供一些系统和逻辑工具，比如spt_reloader：提供热更新,spt_notify：提供notification事件模式，等等。

(这四个组件也可以用于组合成分布式的单服).

每一个组件都对应于单独的项目，你可以从`config/rebar.config`看出.

## 连接协议处理：slg_proto
---
slg_proto设计为处理游戏服务器中的tcp连接和协议打包解包，加密解密，玩家的基本进程运行在此模块.

### 协议规范

在目录proto下定义项目需要的包和协议，需要遵守以下规范：

`api.txt` 定义了服务器中所有的包类型。

* 每个协议的包名需要以:`_req，_ack，_ntf，_cah`结尾，如果违反`make g`时会报错.
* 每个req包必须要有一对应的回复ack包，这两种包用作同步请求处理。
* ntf包用作服务器主动向客户端push的包，比如聊天和一些消息通知，客户端临时使用以下。
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

### 回调处理

`player.erl`模块有以下几个回调函数:

`quit/2:` 进程退出，可以视为玩家退出登陆的事件点。
`cast/2` 处理异步消息，gen_server:cast即可。
`info/2` 处理info，gen_server。
`call/3` 处理call，gen_server。

## gd配置文件处理
---

slg-csv模块用来把gd配置的csv文件导入到ets表。
