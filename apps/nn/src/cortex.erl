%% @author alekras
%% @doc @todo Add description to cortex.


-module(cortex).
-behaviour(gen_server).
-include("nn.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
  start_link/1, 
  applyGenotype/2, 
  extractGenotype/1, 
  updateWeights/2,
  rollbackWeights/2,
  extractWeightsList/1, 
  send_signal_to/2, 
  set_call_back/2,
  set_scape/2
]).

start_link(Cortex_Id) ->
%%  Cortex_Id = list_to_atom(lists:concat(["cortex_", NN_ID])),
  gen_server:start_link({local, Cortex_Id}, ?MODULE, [Cortex_Id], []).

applyGenotype(Pid, Genotype) ->
  gen_server:call(Pid, {genotype, apply, Genotype}).

extractGenotype(Pid) ->
  gen_server:call(Pid, {genotype, extract}).

updateWeights(Pid, List) ->
  gen_server:call(Pid, {update_weights, List}).

rollbackWeights(Pid, Nid) ->
  gen_server:call(Pid, {rollback_weights, Nid}).

extractWeightsList(Pid) ->
  gen_server:call(Pid, extract_weights).

send_signal_to(Pid, Values) ->
  gen_server:cast(Pid, {signal, Values}).

set_call_back(Pid, Callback_Fun) ->
  gen_server:call(Pid, {set_call_back, Callback_Fun}).

set_scape(Pid, Scape_Pid) ->
  gen_server:call(Pid, {set_scape, Scape_Pid}).

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
%  io:format("Cortex init: Neural network Id=~p[pid=~p]~n", [NN_ID,self()]),
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
handle_call({genotype, apply, Genotype}, _From, State) ->
%%  io:format(user, "~nGenotype ~p.~n[Pid=~p]~n", [Genotype,self()]),
  Sup_curr_Pid = State#cortex_state.neuron_supervisor,
  Is_Alive = is_pid(Sup_curr_Pid) andalso is_process_alive(Sup_curr_Pid),
  if
    Is_Alive ->
%      io:format(user, "Neuron supervisor is alive.~n", []),
      Sup_Pid = Sup_curr_Pid,
      [begin supervisor:terminate_child(Sup_curr_Pid, Id), supervisor:delete_child(Sup_curr_Pid, Id) end 
        || {Id, _Pid, _, _} <- supervisor:which_children(Sup_curr_Pid)];
    true ->
%      io:format(user, "Neuron supervisor is not running.~n", []),
      {ok, Sup_Pid} = supervisor:start_link(neuron_sup, [])
  end,
  Ch_Id_Pids = neuron_sup:build_nn(Genotype, Sup_Pid),
  configure(Ch_Id_Pids, Genotype),
  Sensors_Pids = [{NId, proplists:get_value(NId, Ch_Id_Pids)} || #inp_config{type = Type, nid = NId} <- Genotype, Type =:= sensor],
  Neurons_Pids = [{NId, proplists:get_value(NId, Ch_Id_Pids)} || #inp_config{type = Type, nid = NId} <- Genotype, Type =:= neuron],
  Actuators_Pids = [{NId, proplists:get_value(NId, Ch_Id_Pids)} || #inp_config{type = Type, nid = NId} <- Genotype, Type =:= actuator],
  {reply, ok, State#cortex_state{genotype = Genotype, 
                                 neuron_supervisor = Sup_Pid, 
                                 nid_pids = Ch_Id_Pids, 
                                 sensors = Sensors_Pids, 
                                 neurons = Neurons_Pids, 
                                 actuators = Actuators_Pids}};

handle_call({genotype, extract}, _From, #cortex_state{sensors = Sensors, neurons = Neurons, actuators = Actuators} = State) ->
  io:format(user, "~nExtract genotype.~n", []),
  GT0 = [neuron:extract_genom(Pid) || {_Nid, Pid} <- Sensors],
  GT1 = [neuron:extract_genom(Pid) || {_Nid, Pid} <- Neurons],
  GT2 = [neuron:extract_genom(Pid) || {_Nid, Pid} <- Actuators],
  GT = GT0 ++ GT1 ++ GT2,
  {reply, GT, State#cortex_state{genotype = GT}};

handle_call(extract_weights, _From, #cortex_state{neurons = Neurons} = State) ->
  WT = [neuron:extract_weights(Pid) || {_Nid, Pid} <- Neurons],
%  io:format(user, "~nExtract weight list= ~128p.~n", [WT]),
  {reply, WT, State};

handle_call({update_weights, List}, _From, #cortex_state{neurons = Neurons_nid_pidS} = State) ->
%%  io:format(user, ">>> update ~128p  ~128p.~n", [List, Neurons_nid_pidS]),
  [neuron:update_weights(proplists:get_value(Nid, Neurons_nid_pidS), L) || {Nid, L} <- List],
  {reply, ok, State};

handle_call({rollback_weights, Nid}, _From, #cortex_state{neurons = Neurons} = State) ->
%%  io:format(user, ">>> update ~128p  ~128p.~n", [List, Neurons_nid_pidS]),
  Pid = proplists:get_value(Nid, Neurons),
  neuron:rollback_weights(Pid),
  {reply, ok, State};

handle_call({set_call_back, Callback_Fun}, _From, State) ->
  if
    is_function(Callback_Fun, 1) ->
      {reply, ok, State#cortex_state{result_callback = Callback_Fun}};
    true ->
      {reply, error, State}
  end;

handle_call({set_scape, Scape_Pid}, _From, State) ->
  {reply, ok, State#cortex_state{scape_pid = Scape_Pid}};

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
%  [neuron:connect(Pid, OutList, self()) || #out_config{pid = Pid, output = OutList} <- Comp_output].
  neuron_sup:connect_nn(Comp_output, self()).

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
handle_cast({actuator, Nid, Result}, #cortex_state{result_callback = Fun, scape_pid = Scape, actions = Actions, result = St_Result} = State) ->
%  io:format(user, "Cortex[~p] >> message ~128p comes from actuator[~p].~n", [self(), Result, Nid]),
  case lists:keytake(Nid, 1, Actions) of
    {value, {Nid, _}, []} ->
      New_Result = [Result | St_Result],
      New_Actions = [],
      %% @todo send to Scape as gen_server!
      if is_pid(Scape) ->
        Scape ! New_Result;
      true -> ok
      end,
      if is_function(Fun) ->
        Fun(New_Result);
      true -> ok
      end;
    {value, {Nid, _}, New_Actions} ->
      New_Result = [Result | St_Result];
    false ->
      io:format(user, "Wrong message {actuator, ~p, ~p} comes to cortex.~n", [Nid, Result]),
      New_Actions = Actions,
      New_Result = St_Result
  end,
  {noreply, State#cortex_state{actions = New_Actions, result = New_Result}};

handle_cast({signal, Values}, State) ->
%%  io:format(user, "signal comes to cortex ~p.~n    Sensors are ~128p.~n", [Values, State#cortex_state.sensors]),
  [neuron:signal(Pid, -1, proplists:get_value(N, Values, 0.0)) || {N, Pid} <- State#cortex_state.sensors],
  {noreply, State#cortex_state{actions = State#cortex_state.actuators, result = []}};

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

handle_info(_Info, State) ->
  io:format(user, "unknown request comes to cortex ~p.~n", [_Info]),
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


