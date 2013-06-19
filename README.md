# 初始化

*获取代码*
    git clone git@42.121.105.8:slg-server.git

*获取依赖关系*
    make deps

*生成协议代码*
    make g

*初始化数据库*
    make db

*启动* 默认用户名加密码为：root，密码空；如果不是，自行修改slg_server.erl
    make s

*测试*
    在新的终端执行`make e`，执行:
    gt:start().

本项目是为小服设计的slg游戏服务器框架，主要包含以下四个组件:

* slg_proto:处理网络连接，数据的序列化，加密解密处理.
* slg_csv:游戏中常常有gd配置文件，slg_csv提供配置文件到ets表的直接映射，便于编程访问.
* slg_model:本框架使用ets在内存中cache玩家数据，slg_model提供ets和mysql的表映射，提供cache和自动写回等功能。
* slg_support:提供一些系统和逻辑工具，比如spt_reloader：提供热更新,spt_notify：提供notification事件模式，等等。

(这四个组件也可以用于组合成分布式的单服).

每一个组件都对应于单独的项目，你可以从config/rebar.config看出.
