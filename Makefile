# 需要加载的代码路径
LOAD_PATH = \
	ebin \
	deps/*/ebin \
	$(NULL)

# 节点名称
NODE=$(shell cat ./config/node_name.conf)
ifeq ($(NODE),)
	NODE = slg-server@127.0.0.1
endif

# cookie
COOKIE = abcdeft

# 部分配置参数
OPTS = \
	-pa $(LOAD_PATH) \
	-env ERL_MAX_ETS_TABLES 10000 \
	-setcookie $(COOKIE) \
	+A 8 +K true +P 120000  # -smp disable \
	-detached  \
	-noshell \
	$(NULL)

# rebar-用于编译
REBAR := ./bin/rebar --config config/rebar.config
UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
REBAR := ./bin/rebar.linux --config config/rebar.config
# do something Linux-y
endif

# 编译全部
all:
	$(REBAR) compile

# 获取到所有的依赖
deps:
	$(REBAR) get-deps

pl:
	cd deps/slg_csv && git pull origin master
	cd deps/slg_proto && git pull origin master
	cd deps/slg_support && git pull origin master
	cd deps/slg_model && git pull origin master

ps:
	cd deps/slg_csv && git push origin master
	cd deps/slg_proto && git push origin master
	cd deps/slg_support && git push origin master
	cd deps/slg_model && git push origin master

t:
	$(REBAR) compile eunit

c:
	$(REBAR) clean

# 调用生成器生成代码
g:
	cd deps/slg_proto/src && ruby ./proto_gen.rb $(shell pwd)/
	cp include/proto*  deps/slg_proto/include

s:
	erl $(OPTS) -name $(NODE) -s slg_server

e:
	erl $(OPTS)

r:
	erl $(OPTS) -s robot start

# 连接上后台erlang节点
m:
	erl $(OPTS) -name deamon_shell@127.0.0.1  -remsh $(NODE)

db:
	erl $(OPTS) -s slg_server migrate_do -noshell


.PHONY: deps get-deps
