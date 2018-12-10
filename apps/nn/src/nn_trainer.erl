%% @author alekras
%% @doc @todo Add description to nn_trainer.

-module(nn_trainer).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
  run_step/3,
  run_loop/5,
  incNth/3
]).
-warning("...compiling nn_trainer.").

run_loop(Cortex_Id, Impact, Deltas, Error_dsr, N) ->
  run_loop(Cortex_Id, Impact, Deltas, Error_dsr, N, [{-1,0,0.0,10.0},{-1,0,0.0,10.0},{-1,0,0.0,10.0}]).

run_loop(_Cortex_Id, _Impact, _Deltas, _Err, 0, _History) -> stop;
run_loop(Cortex_Id, Impacts, Deltas, Error, N, [{Pr_nid, Pr_Nw, Pr_Dlt, Pr_err} = Pr1, Pr2, _Pr3] = History) ->
  io:format(user, "Step[~p] error=~p.~n", [N, Error]),
  B = run_step(Cortex_Id, Impacts, Deltas),
  {Nid, Nw, Dlt, Err} = B,
  io:format(user, "    [~p] NID=~p, N=~p, Delta=~p, Err=~p.~n", [N, Nid, Nw, Dlt, Err]),
%  io:format(user, "Weights after Train: ~128p.~n", [cortex:extractWeightsList(cortex_1)]),
  if (Err > Error) and (Err < (Pr_err - Error * 0.1)) ->
    run_loop(Cortex_Id, Impacts, Deltas, Error, N - 1, [B, Pr1, Pr2]);
  (Err > Error) and (Err >= (Pr_err - Error * 0.1)) ->
    io:format(user, "Change Delta:    ~p.~n", [lists:map(fun(A) -> A * 0.5 end, Deltas)]),
    run_loop(Cortex_Id, Impacts, lists:map(fun(A) -> A * 0.5 end, Deltas), Error, N - 1, [B, Pr1, Pr2]);
  (Err < Error) ->
    done;
  true ->
    io:format(user, "Deltas=~p, err=~p, Prev err=~p.~n", [Deltas, Err, (Pr_err - Error * 0.1)]),
    false
  end.

%% run_impact_step(_Cortex_Id, [], _Delta, Error) ->
%%   math:sqrt(Error);
%% run_impact_step(Cortex_Id, [{Sensor_Signals, Goal} | Impact], Delta, Error) ->
%%   B = run_step(Cortex_Id, Sensor_Signals, Goal, Delta),
%%   {_,_,_, Err} = B,
%%   io:format(user, "Impact Step: goal=~p, Error=~p.~n", [Goal, B]),
%%   run_impact_step(Cortex_Id, Impact, Delta, Error + (Err * Err)).

external_impact(_Cortex_Id, [], Error) ->
  math:sqrt(Error);
external_impact(Cortex_Id, [{Sensor_Signals, Goal} | Impact], Error) ->
  cortex:send_signal_to(Cortex_Id, Sensor_Signals),
  Err =
  receive
    Res when is_list(Res) ->
      abs(Goal - lists:sum(lists:flatten(Res)))
  end,
%%  io:format(user, "  Impact Step: goal=~p, Error=~p, Res=~128p, Sum=~p.~n", [Goal, Err, Res, lists:sum(lists:flatten(Res))]),
  external_impact(Cortex_Id, Impact, Error + (Err * Err)).

run_step(Cortex_Id, Impacts, Deltas) ->
  cortex:set_scape(Cortex_Id, self()),
  LT =
  [[begin
      LD =
      [begin
         cortex:updateWeights(Cortex_Id, [{Nid, incNth(N, D, WL)}]),
         Err1 = external_impact(Cortex_Id, Impacts, 0),
%         io:format(user, "    Impact Step: NID=~p, Delta=~p, Error=~p, WL=~128p.~n", [Nid, D, Err1, cortex:extractWeightsList(Cortex_Id, Nid)]),
         {Nid, N, D, Err1}
       end || D <- Deltas],
%%       cortex:updateWeights(Cortex_Id, [{Nid, incNth(N, Delta, WL)}]),
%%       Err1 = external_impact(Cortex_Id, Impacts, 0),
%%      io:format(user, "    Impact Step: NID=~p, Delta=~p, Error=~p, WL=~128p.~n", [Nid, Delta, Err1, cortex:extractWeightsList(Cortex_Id, Nid)]),
%%       cortex:rollbackWeights(Cortex_Id, Nid),

%%       cortex:updateWeights(Cortex_Id, [{Nid, incNth(N, -Delta, WL)}]),
%%       Err2 = external_impact(Cortex_Id, Impacts, 0),
%%      io:format(user, "    Impact Step: NID=~p, Delta=~p, Error=~p, WL=~128p.~n", [Nid, -Delta, Err2, cortex:extractWeightsList(Cortex_Id, Nid)]),
%%      cortex:rollbackWeights(Cortex_Id, Nid),

%      io:format(user, "~nReceived results:= ~p, ~p.", [R3, R4]),
      cortex:updateWeights(Cortex_Id, [{Nid, WL}]),
      Choice = lists:sort(fun({_, _, _, Va}, {_, _, _, Vb}) -> (Va < Vb) end, LD),
%%      if Err1 > Err2 ->
%%        {Nid, N, -Delta, Err2};
%%      true ->
%%        {Nid, N, Delta, Err1}
%%      end,
%%      io:format(user, "    Impact Step: {NID, N, Delta, Error} =~256p, WL=~256p.~n", [Choice, cortex:extractWeightsList(Cortex_Id, Nid)]),
      hd(Choice)
    end || N <- lists:seq(0, length(WL)-1)] || {Nid, WL, _Bias} <- cortex:extractWeightsList(Cortex_Id)],
%  io:format(user, "~nLT := ~128p.~n", [lists:flatten(LT)]),
  [P0, P1 | _] = lists:sort(fun({_, _, _, Va}, {_, _, _, Vb}) -> (Va < Vb) end, lists:flatten(LT)),
  [begin 
     {_Nid, WTs, _Bias} = cortex:extractWeightsList(Cortex_Id, Nid),
     cortex:updateWeights(Cortex_Id, [{Nid, incNth(Nw, Dlt, WTs)}])
   end || {Nid, Nw, Dlt, _} <- [P0]], %%[P0, P1]],
  P0.

incNth(N, Dlt, List) ->
  incNth(N, Dlt, List, []).

incNth(_N, _Dlt, [], Rslt) ->
  lists:reverse(Rslt);
incNth(0, Dlt, [H | List], Rslt) ->
  incNth(-1, Dlt, List, [(H + Dlt) | Rslt]);
incNth(N, Dlt, [H | List], Rslt) ->
  incNth(N - 1, Dlt, List, [H | Rslt]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {}).

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
init([]) ->
  {ok, #state{}}.

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
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

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


