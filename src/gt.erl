-module(gt).
-compile([export_all]).

-include("proto.hrl").

%% 客户端数据结构体
-record(state, {
          socket, %%套接字
          socket2
         }).

start() ->
  {ok, Pid} = start_link("127.0.0.1", 4000),
  ?MODULE:login(Pid, "nice1", "few"),
  Pid.

%% api
start_link(IP, Port) ->
  gen_server:start_link(?MODULE, [IP, Port], []).

register(Pid, Name, Udid) ->
  gen_server:call(Pid, {register, Name, Udid}).

login(Pid, Name, Udid) ->
  gen_server:call(Pid, {login, Name, Udid}).

building_up(Pid, Type) ->
  gen_server:call(Pid, {building_up, Type}).

building_l_up(Pid, Id) ->
  gen_server:call(Pid, {building_l_up, Id}).

building_d(Pid, Id) ->
  gen_server:call(Pid, {building_d, Id}).

stop(Ref) ->
  gen_server:cast(Ref, stop).

init([IP, Port]) ->
  {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {packet, 2}, {active, true}]),
  {ok, #state{socket = Socket}}.

handle_cast(_, State) ->
  {noreply, State}.

handle_call({building_up, Type}, _From, State=#state{socket=Socket}) ->
  Pt = #pt_building{b_type=Type},
  proto_request:building_up_req(Pt, Socket),
  {reply, ok, State};


handle_call({building_d, Id}, _From, State=#state{socket=Socket}) ->
  Pt = #pt_building{id=Id},
  proto_request:building_del_req(Pt, Socket),
  {reply, ok, State};

handle_call({building_l_up, Id}, _From, State=#state{socket=Socket}) ->
  Pt = #pt_building{id=Id},
  proto_request:building_upl_req(Pt, Socket),
  {reply, ok, State};

handle_call({register, Name, Udid}, _From, State=#state{socket=Socket}) ->
  Base = #pt_ubase{name=Name, sex=1},
  Device = #db_device{udid=Udid},
  proto_request:register_req(#pt_account{base=Base, device=Device}, Socket),
  {reply, ok, State};

handle_call({login, Name, Udid}, _From, State=#state{socket=Socket}) ->
  Base = #pt_ubase{name=Name, sex=1},
  Device = #db_device{udid=Udid},
  proto_request:login_req(#pt_account{base=Base, device=Device}, Socket),
  {reply, ok, State};

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_info({tcp, _Socket, Bin}, State) ->
  <<Type:?HWORD, RequestData/binary>> = Bin,
  ?M(Payload) = proto_decoder:decode(Type, RequestData),
  io:format("~p~n", [Payload]),
  {noreply, State};

handle_info(_Info, State) ->
  {stop, normal, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
terminate(_Reason, _State) ->
  ok.

