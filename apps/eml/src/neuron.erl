%% @author alekras
%% @doc @todo Add description to neuron.

-module(neuron).
-behaviour(gen_server).
-include("nn.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([neuron/1, sensor/1, actuator/1, connect/3, signal/3]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

neuron(Config) ->
  gen_server:start_link(?MODULE, Config, []).

sensor(Config) ->
  gen_server:start_link(?MODULE, Config, []).

actuator(Config) ->
  gen_server:start_link(?MODULE, Config, []).

connect(Pid, OutList, Cortex_Id) ->
  gen_server:call(Pid, {connect, OutList, Cortex_Id}).

signal(Pid, CallerNid, Input) ->
  gen_server:call(Pid, {signal, CallerNid, Input}).

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
init(#inp_config{type = neuron, nid = NeuronId, bias = Bias, input = InputList}) ->
	{ok, #state{nid = NeuronId, component_type = neuron, cortes_pid = undefined, input = InputList, output = [], bias = Bias, accum = 0, signals = []}};

init(#inp_config{type = sensor, nid = NeuronId, bias = Bias, input = InputList}) ->
	{ok, #state{nid = NeuronId, component_type = sensor, cortes_pid = undefined, input = InputList, output = [], bias = Bias, accum = undefined, signals = []}};

init(#inp_config{type = actuator, nid = NeuronId, bias = Bias, input = InputList}) ->
	{ok, #state{nid = NeuronId, component_type = actuator, cortes_pid = underfined, input = InputList, output = [], bias = Bias, accum = [], signals = []}}.

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
handle_call({connect, Outlist}, _From, State) ->
  {reply, ok, State#state{output = Outlist}};

handle_call({signal, _CallerNid, Input}, _From, #state{component_type = sensor} = State) ->
  [signal(Out_Pid, Out_Nid, Input) || #out_item{nid = Out_Nid, pid = Out_Pid} <- State#state.output],
  {reply, ok, State};

handle_call({signal, CallerNid, Input}, _From, #state{component_type = neuron, accum = Accum, signals = Signals} = State) ->
  case lists:keytake(CallerNid, 1, Signals) of
    {value, {CallerNid, Weight}, []} ->
      New_accum = Accum + Input * Weight + State#state.bias,
      [signal(Out_Pid, Out_Nid, math:tanh(New_accum)) || #out_item{nid = Out_Nid, pid = Out_Pid} <- State#state.output],
	  New_signals = State#state.input;
    {value, {CallerNid, Weight}, New_signals} ->
      New_accum = Accum + Input * Weight;
    false ->
      io:format("Wrong message {signal, ~p, ~p} comes to neuron.~n", [CallerNid, Input]),
      New_signals = Signals,
      New_accum = Accum
  end,
  {reply, ok, State#state{accum = New_accum, signals = New_signals}};

handle_call({signal, CallerNid, Input}, _From, #state{component_type = actuator, accum = Accum, signals = Signals} = State) ->
  case lists:keytake(CallerNid, 1, Signals) of
    {value, {CallerNid, _Weight}, []} ->
      New_accum = [Input | Accum],
      State#state.cortes_pid ! {actuator, New_accum},
	  New_signals = State#state.input;
    {value, {CallerNid, _Weight}, New_signals} ->
      New_accum = [Input | Accum];
    false ->
      io:format("Wrong message {signal, ~p, ~p} comes to actuator.~n", [CallerNid, Input]),
      New_signals = Signals,
      New_accum = Accum
  end,
  {reply, ok, State#state{accum = New_accum, signals = New_signals}};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


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


