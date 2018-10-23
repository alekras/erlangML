%% @author axk456
%% @doc @todo Add description to cortex.


-module(cortex).
-behaviour(gen_server).
-include("nn.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, applyGenotype/2]).

start_link(NN_ID) ->
  Cortex_Id = list_to_atom(lists:concat(["cortex_", NN_ID])),
  gen_server:start_link({local, Cortex_Id}, ?MODULE, [NN_ID], []).

applyGenotype(Pid, Genotype) ->
  gen_server:call(Pid, {genotype, Genotype}).

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
handle_call({actuator, Result}, _From, State) ->
  io:format(user, "message comes to cortex ~p.~n", [Result]),
  {reply, ok, State};

handle_call({genotype, Genotype}, _From, State) ->
  io:format(user, "Genotype ~p.[Pid=~p]~n", [Genotype,self()]),
  Is_Alive = is_process_alive(State#cortex_state.neuron_supervisor),
  if
    Is_Alive ->
      io:format(user, "Neuron supervisor is alive.~n", []),
      Sup_Pid = State#cortex_state.neuron_supervisor,
      [begin supervisor:terminate_child(State#cortex_state.neuron_supervisor, Id), supervisor:delete_child(State#cortex_state.neuron_supervisor, Id) end 
        || {Id, _Pid, _, _} <- supervisor:which_children(State#cortex_state.neuron_supervisor)];
    true ->
      io:format(user, "Neuron supervisor is not running.~n", []),
      {ok, Sup_Pid} = supervisor:start_link(neuron_sup, [])
  end,
  Neuron_Child_Spec = [{NId, {neuron, Type, [Config]}, permanent, 2000, worker, [neuron]} || #inp_config{type = Type, nid = NId} = Config <- Genotype],
  Ch_Pids = [{NId, supervisor:start_child(Sup_Pid, Ch_Spec)} || {NId, _, _, _, _, _} = Ch_Spec <- Neuron_Child_Spec],
  configure(Ch_Pids, Genotype),
  {reply, ok, State#cortex_state{genotype = Genotype}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

configure(Ch_Pids, Genotype) ->
  Id_Pid = [{Id, Pid} || {Id, {ok, Pid}} <- Ch_Pids],
%  io:format(user, "Id_Pid: ~128p cortex Pid=~p~n", [Id_Pid, Cortex_Pid]),
%  io:format(user, "Configuration: ~128p ~n", [ConfList]),
  Comp_output_temp = [#out_config{nid = Nid, pid = proplists:get_value(Nid, Id_Pid), output = []} || #inp_config{nid = Nid} <- Genotype],
%  io:format(user, "Comp_output_temp: ~128p ~n", [Comp_output_temp]),
  Temp = lists:flatten([[{Nid, Inp_item} || Inp_item <- InpList] || #inp_config{nid = Nid, input = InpList} <- Genotype]),
%  io:format(user, "Temp: ~128p ~n", [Temp]),
  Comp_output = process(Comp_output_temp, Id_Pid, Temp),
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
    {noreply, State};
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


