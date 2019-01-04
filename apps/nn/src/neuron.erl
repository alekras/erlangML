%% @author alekras
%% @doc @todo Add description to neuron.

-module(neuron).
-behaviour(gen_server).
-include("nn.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
  neuron/1,
%%   sensor/1, 
%%   actuator/1, 
  connect/3, 
  signal/3, 
  update_weights/3,
  perturb_weights/1,
  rollback_weights/1, 
  extract_genom/1, 
  extract_weights/1
]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

neuron(Config) ->
  gen_server:start_link(?MODULE, Config, []).

%% sensor(Config) ->
%%   gen_server:start_link(?MODULE, Config, []).
%% 
%% actuator(Config) ->
%%   gen_server:start_link(?MODULE, Config, []).

connect(Pid, OutList, Cortex_Id) ->
  gen_server:cast(Pid, {connect, OutList, Cortex_Id}).

signal(Pid, CallerNid, Input) ->
  gen_server:cast(Pid, {signal, CallerNid, Input}).

update_weights(Pid, Weight_List, Bias) ->
  gen_server:call(Pid, {update, Weight_List, Bias}).

perturb_weights(Pid) ->
  gen_server:call(Pid, perturb).

rollback_weights(Pid) ->
  gen_server:call(Pid, rollback).

extract_genom(Pid) ->
  gen_server:call(Pid, extract_genom).

extract_weights(Pid) ->
  gen_server:call(Pid, extract_weights).

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
  {ok, #state{nid = NeuronId, component_type = neuron, cortes_pid = undefined, input = InputList, output = [], bias = Bias, accum = 0, signals = InputList}};

init(#inp_config{type = sensor, nid = NeuronId, bias = Bias}) ->
  {ok, #state{nid = NeuronId, component_type = sensor, cortes_pid = undefined, input = [], output = [], bias = Bias, accum = undefined, signals = []}};

init(#inp_config{type = actuator, nid = NeuronId, bias = Bias, input = InputList}) ->
  {ok, #state{nid = NeuronId, component_type = actuator, cortes_pid = underfined, input = InputList, output = [], bias = Bias, accum = [], signals = InputList}}.

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

handle_call({update, Weight_List, New_Bias}, _From, #state{component_type = neuron, input = Input, bias = Bias} = State) ->
%%  io:format("Neuron[~p] Update weights: Old W= ~128p New W=~128p.~n", [State#state.nid, Input, Weight_List]),
  New_Input = update(Input, Weight_List),
%%  io:format("New_Input= ~128p.~n", [New_Input]),
  {reply, ok, State#state{input = New_Input, input_bak = Input, signals = New_Input, bias = New_Bias, bias_bak = Bias}};

handle_call(perturb, _From, #state{component_type = neuron, input = Input, bias = Bias} = State) ->
%%  io:format("Neuron[~p] Update weights: Old W= ~128p New W=~128p.~n", [State#state.nid, Input, Weight_List]),
  P = 1 / math:sqrt(length(Input) + 1),
  New_Input = perturb(P, Input),
%  New_Bias = perturb(P, Bias),
  New_Bias = Bias,
%%  io:format("New_Input= ~128p.~n", [New_Input]),
  {reply, ok, State#state{input = New_Input, input_bak = Input, signals = New_Input, bias = New_Bias, bias_bak = Bias}};

handle_call(rollback, _From, #state{component_type = neuron, input_bak = []} = State) ->
  {reply, ok, State};
handle_call(rollback, _From, #state{component_type = neuron, input_bak = InputBak, bias_bak = Bias} = State) ->
  {reply, ok, State#state{input = InputBak, signals = InputBak, input_bak = [], bias = Bias, bias_bak = 0.0}};

handle_call(extract_genom, _From, #state{component_type = Type, nid = Nid, input = Input, bias = Bias} = State) ->
  Genom = #inp_config{type = Type, nid = Nid, bias = Bias, input = Input},
  {reply, Genom, State};

handle_call(extract_weights, _From, #state{nid = Nid, input = Input, bias = Bias} = State) ->
  Weights = {Nid, [W || #inp_item{weight = W} <- Input], Bias},
%%  io:format("Extract Weights= ~128p.~n", [Weights]),
  {reply, Weights, State};

handle_call(_Request, _From, State) ->
  io:format(user, "unknown request comes to neuron ~p.~n", [_Request]),
  {reply, ok, State}.

update(L1, L2) ->
  update(L1, L2, []).

update([], _, L3) -> lists:reverse(L3);
update(_, [], L3) -> lists:reverse(L3);
update([Inp_Item | L1], [New_W | L2], L3) ->
  update(L1, L2, [Inp_Item#inp_item{weight = New_W} | L3]).

perturb(P, Value) when is_float(Value) ->
  RV = rand:uniform(),
  case RV < P of
    true ->
      V = Value + (rand:uniform() - 0.5) * ?DELTA_MULTIPLIER,
      if
        V < -?SAT_LIMIT -> -?SAT_LIMIT;
        V > ?SAT_LIMIT -> ?SAT_LIMIT;
        true -> V
      end;
     false -> Value
  end;
perturb(P, InpList) when is_list(InpList) ->
  lists:map(fun(#inp_item{weight = W} = Inp) -> Inp#inp_item{weight = perturb(P, W)} end, InpList).

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
handle_cast({connect, OutList, Cortex_Id}, #state{} = State) ->
%  io:format("Connect: nid=~p type=~p ~128p ~n", [State#state.nid, State#state.component_type, OutList]),
  {noreply, State#state{output = OutList, cortes_pid = Cortex_Id}};

handle_cast({signal, _CallerNid, Input}, #state{component_type = sensor} = State) ->
%%  io:format("Sensor[~p] >>> signal {~p, ~p}.~n", [State#state.nid, _CallerNid, Input]),
  [signal(Out_Pid, State#state.nid, Input) || #out_item{pid = Out_Pid} <- State#state.output],
  {noreply, State};

handle_cast({signal, CallerNid, Input}, #state{component_type = neuron, accum = Accum, signals = Signals} = State) ->
%%  io:format("Neuron[~p] >>> signal {~p, ~7.3f}  Signals list: ~256p.~n", [State#state.nid, CallerNid, Input, Signals]),
  case lists:keytake(CallerNid, #inp_item.nid, Signals) of
    {value, #inp_item{nid = CallerNid, weight = Weight}, []} ->
      Final_accum = Accum + Input * Weight + State#state.bias,
      [signal(Out_Pid, State#state.nid, math:tanh(Final_accum)) || #out_item{nid = _Out_Nid, pid = Out_Pid} <- State#state.output],
      New_signals = State#state.input,
      New_accum = 0;
    {value, #inp_item{nid = CallerNid, weight = Weight}, New_signals} ->
      New_accum = Accum + Input * Weight;
    false ->
      New_accum = Accum,
      New_signals = Signals,
      io:format("Wrong message {signal, ~p, ~p} comes to neuron nid=~p.~n", [CallerNid, Input, State#state.nid])
  end,
  {noreply, State#state{accum = New_accum, signals = New_signals}};

handle_cast({signal, CallerNid, Input}, #state{nid = Nid, component_type = actuator, accum = Accum, signals = Signals} = State) ->
%  io:format(user, "Actuator[~p] >>> signal {~p, ~p}. list= ~p.~n", [State#state.nid, CallerNid, Input, Signals]),
  case lists:keytake(CallerNid, #inp_item.nid, Signals) of
    {value, #inp_item{nid = CallerNid}, []} ->
      Final_accum = [Input | Accum],
      gen_server:cast(State#state.cortes_pid, {actuator, Nid, Final_accum}),
      New_signals = State#state.input,
      New_accum = [];
    {value, #inp_item{nid = CallerNid}, New_signals} ->
      New_accum = [Input | Accum];
    false ->
      io:format("Wrong message {signal, ~p, ~p} comes to actuator nid=~p.~n", [CallerNid, Input, State#state.nid]),
      New_signals = Signals,
      New_accum = Accum
  end,
  {noreply, State#state{accum = New_accum, signals = New_signals}};

handle_cast(_Msg, State) ->
  io:format(user, "cast unexpected message came ~p.~n", [_Msg]),
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


