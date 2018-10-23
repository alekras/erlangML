%% @author axk456
%% @doc @todo Add description to cortex.


-module(cortex).
-behaviour(gen_server).
-include("nn.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, applyGenotype/2, send_signal_to/3, result/1]).

start_link(NN_ID) ->
  Cortex_Id = list_to_atom(lists:concat(["cortex_", NN_ID])),
  gen_server:start_link({local, Cortex_Id}, ?MODULE, [NN_ID], []).

applyGenotype(Pid, Genotype) ->
  gen_server:call(Pid, {genotype, Genotype}).

send_signal_to(Pid, Nid, Val) ->
  gen_server:cast(Pid, {signal, Nid, Val}).

result(Pid) ->
  gen_server:call(Pid, result).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init(NN_ID) ->
  io:format("Cortex init: Neural network Id=~p[pid=~p]~n", [NN_ID,self()]),
  {ok, #cortex_state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({genotype, Genotype}, _From, State) ->
  io:format(user, "~nGenotype ~p.~n[Pid=~p]~n", [Genotype,self()]),
  Sup_curr_Pid = State#cortex_state.neuron_supervisor,
  Is_Alive = is_pid(Sup_curr_Pid) andalso is_process_alive(Sup_curr_Pid),
  if
    Is_Alive ->
      io:format(user, "Neuron supervisor is alive.~n", []),
      Sup_Pid = Sup_curr_Pid,
      [begin supervisor:terminate_child(Sup_curr_Pid, Id), supervisor:delete_child(Sup_curr_Pid, Id) end 
        || {Id, _Pid, _, _} <- supervisor:which_children(Sup_curr_Pid)];
    true ->
      io:format(user, "Neuron supervisor is not running.~n", []),
      {ok, Sup_Pid} = supervisor:start_link(neuron_sup, [])
  end,
  Neuron_Child_Spec = [{NId, {neuron, Type, [Config]}, permanent, 2000, worker, [neuron]} || #inp_config{type = Type, nid = NId} = Config <- Genotype],
  Ch_Id_Pids =[{Ch_Id, Ch_Pid} || {Ch_Id, {ok, Ch_Pid}} <- [{NId, supervisor:start_child(Sup_Pid, Ch_Spec)} || {NId, _, _, _, _, _} = Ch_Spec <- Neuron_Child_Spec]],
  configure(Ch_Id_Pids, Genotype),
  {reply, ok, State#cortex_state{genotype = Genotype, neuron_supervisor = Sup_Pid, id_pids = Ch_Id_Pids}};

handle_call(result, _From, State) ->
  {reply, State#cortex_state.result, State};

handle_call(_Request, _From, State) ->
  io:format(user, "unknown request comes to cortex ~p.~n", [_Request]),
  {reply, ok, State}.

configure(Ch_Pids, Genotype) ->
%  io:format(user, "Id_Pid: ~128p cortex Pid=~p~n", [Id_Pid, Cortex_Pid]),
%  io:format(user, "Configuration: ~128p ~n", [ConfList]),
  Comp_output_temp = [#out_config{nid = Nid, pid = proplists:get_value(Nid, Ch_Pids), output = []} || #inp_config{nid = Nid} <- Genotype],
%  io:format(user, "Comp_output_temp: ~128p ~n", [Comp_output_temp]),
  Temp = lists:flatten([[{Nid, Inp_item} || Inp_item <- InpList] || #inp_config{nid = Nid, input = InpList} <- Genotype]),
%  io:format(user, "Temp: ~128p ~n", [Temp]),
  Comp_output = process(Comp_output_temp, Ch_Pids, Temp),
%  io:format(user, "Comp_output: ~128p ~n", [Comp_output]),
  [neuron:connect(Pid, OutList, self()) || #out_config{pid = Pid, output = OutList} <- Comp_output].

process(O, _, []) -> O;
process(O, Id_Pid_list, [{Nid_O, #inp_item{nid = Nid_I}} | T]) ->
  case lists:keyfind(Nid_I, #out_config.nid, O) of
    #out_config{output = Out} = Out_cnfg ->
      Pid = proplists:get_value(Nid_O, Id_Pid_list),
      New_Out = [#out_item{nid = Nid_O, pid = Pid} | Out],
      New_Out_cnfg = Out_cnfg#out_config{output = New_Out},
      New_O = lists:keyreplace(Nid_I, #out_config.nid, O, New_Out_cnfg);
    false ->
      New_O = O
  end,
  process(New_O, Id_Pid_list, T).

%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({actuator, Result}, State) ->
  io:format(user, "cast: message comes to cortex ~p.~n", [Result]),
  {noreply, State#cortex_state{result = Result}};

handle_cast({signal, Nid, Val}, State) ->
  io:format(user, "signal comes to cortex ~p/~p.~n", [Nid, Val]),
  Pid = proplists:get_value(Nid, State#cortex_state.id_pids),
  neuron:signal(Pid, -1, Val),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================

handle_info({actuator, Result}, State) ->
  io:format(user, "handle_info: message comes to cortex ~p.~n", [Result]),
    {noreply, State#cortex_state{result = Result}};
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


