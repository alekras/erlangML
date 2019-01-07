%% @author alekras
%% @doc @todo Add description to nn_trainer.

-module(nn_trainer).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
  run_loop/5,
  run_loop_pt/5
]).
-warning("...compiling nn_trainer.").

run_loop_pt(_Cortex_Id, _Impacts, _Error_dsr, 0, _M) -> stop;
run_loop_pt(Cortex_Id, Impacts, Error_dsr, N, M) ->
  cortex:set_scape(Cortex_Id, self()),
  io:format(user, "Step[~p].~n", [N]),
  Err0 = external_impact(Cortex_Id, Impacts, 0),
  {Err, Mf} = run_step_pt(Cortex_Id, Impacts, Err0, M),
  io:format(user, "    [~p] Err=~p, Iter N=~p.~n", [N, Err, Mf]),
  io:format(user, "    Weights after run_step_pt:~n~s.~n", [print_weigths(cortex:extractWeightsList(Cortex_Id))]),
  if (Err > Error_dsr) ->
    run_loop_pt(Cortex_Id, Impacts, Error_dsr, N - 1, M);
  (Err < Error_dsr) ->
    done;
  true ->
    false
  end.

run_loop(Cortex_Id, Impact, Deltas, Error_dsr, N) ->
  cortex:set_scape(Cortex_Id, self()),
  run_loop(Cortex_Id, Impact, Deltas, Error_dsr, N, [{-1,0,0.0,10.0},{-1,0,0.0,10.0},{-1,0,0.0,10.0}]).

run_loop(_Cortex_Id, _Impact, _Deltas, _Err, 0, _History) -> stop;
run_loop(Cortex_Id, Impacts, Deltas, Error, N, [{Pr_nid, _Pr_Nw, _Pr_Dlt, Pr_err} = Pr1, Pr2, _Pr3] = _History) ->
  io:format(user, "Step[~p] error=~p.~n", [N, Error]),
  B = run_step(Cortex_Id, Impacts, Deltas),
  {Nid, Nw, Dlt, Err} = B,
  io:format(user, "    [~p] NID=~p, N=~p, Delta=~p, Err=~p.~n", [N, Nid, Nw, Dlt, Err]),
  io:format(user, "Weights after Train:~n~s.~n", [print_weigths(cortex:extractWeightsList(Cortex_Id))]),
  if (Err > Error) and (Err < Pr_err) ->
    run_loop(Cortex_Id, Impacts, Deltas, Error, N - 1, [B, Pr1, Pr2]);
  (Err > Error) and (Nid =:= Pr_nid) ->
    io:format(user, "Change Delta: Curr Nid = ~p[~p], Curr err = ~p, Prev err = ~p, deltas = ~128p.~n", [Nid, Pr_nid, Err, Pr_err, lists:map(fun(A) -> A * 0.5 end, Deltas)]),
    cortex:rollbackWeights(Cortex_Id, Nid),
    run_loop(Cortex_Id, Impacts, lists:map(fun(A) -> A * 0.5 end, Deltas), Error, N - 1, [B, Pr1, Pr2]);
  (Err > Error) and (Nid =/= Pr_nid) ->
    io:format(user, "Change Delta: Curr Nid = ~p[~p], Curr err = ~p, Prev err = ~p, deltas = ~128p.~n", [Nid, Pr_nid, Err, Pr_err, lists:map(fun(A) -> A * 2.0 end, Deltas)]),
    cortex:rollbackWeights(Cortex_Id, Nid),
    run_loop(Cortex_Id, Impacts, lists:map(fun(A) -> A * 2.0 end, Deltas), Error, N - 1, [B, Pr1, Pr2]);
  (Err < Error) ->
    done;
  true ->
    io:format(user, "Deltas=~p, err=~p, Prev err=~p, Prev=~p.~n", [Deltas, Err, (Pr_err + Error * 0.1), Pr1]),
    false
  end.

external_impact(_Cortex_Id, [], Error) ->
%  io:format(user, "    Impact Step completed for all goals, Error=~p.~n", [math:sqrt(Error)]),
%  io:format(user, "    Weights after impact step:~n~s", [print_weigths(cortex:extractWeightsList(_Cortex_Id))]),
  math:sqrt(Error);
external_impact(Cortex_Id, [{Sensor_Signals, Goal} | Impact], Error) ->
  cortex:send_signal_to(Cortex_Id, Sensor_Signals),
  Err =
  receive
    Res when is_list(Res) ->
      LR = lists:flatten(Res),
      E = abs(Goal - lists:sum(LR)), %% / length(LR)),
%%      io:format(user, "  --  Impact Step: goal=~p, Error=~p, Sum=~p.~n", [Goal, E, (lists:sum(LR) / length(LR))]),
%%      io:format(user, "  --  Impact Step: goal=~p, Error=~p, Sum=~p.~n", [Goal, E, lists:sum(LR)]),
      E
  end,
  external_impact(Cortex_Id, Impact, Error + (Err * Err)).

print_floats([W | []]) ->
  io_lib:format("~5.2f", [W]);
print_floats([W | WL]) ->
  io_lib:format("~5.2f,", [W]) ++ print_floats(WL).

print_weigths([]) -> "";
print_weigths([{Nid, WL, B} | L]) ->
  lists:concat(["{", Nid, ",[", print_floats(WL), "],", io_lib:format("~5.2f", [B]), "}\n"]) ++ print_weigths(L).

run_step(Cortex_Id, Impacts, Deltas) ->
  LT =
  [[case N of
      -1 ->
      LD =
      [begin
         cortex:updateWeights(Cortex_Id, [{Nid, WL, Bias + D}]),
         Err = external_impact(Cortex_Id, Impacts, 0),
%         io:format(user, "    Impact Step: NID=~p, Delta=~p, Error=~p, WL=~128p.~n", [Nid, D, Err1, cortex:extractWeightsList(Cortex_Id, Nid)]),
         {Nid, N, D, Err}
       end || D <- Deltas],
      cortex:updateWeights(Cortex_Id, [{Nid, WL, Bias}]),
      Choice = lists:sort(fun({_, _, _, Va}, {_, _, _, Vb}) -> (Va < Vb) end, LD),
%%      io:format(user, "    Impact Step: {NID, N, Delta, Error} =~256p, WL=~256p.~n", [Choice, cortex:extractWeightsList(Cortex_Id, Nid)]),
      hd(Choice);
      _  ->
      LD =
      [begin
         cortex:updateWeights(Cortex_Id, [{Nid, incNth(N, D, WL), Bias}]),
         Err = external_impact(Cortex_Id, Impacts, 0),
%         io:format(user, "    Impact Step: NID=~p, Delta=~p, Error=~p, WL=~128p.~n", [Nid, D, Err1, cortex:extractWeightsList(Cortex_Id, Nid)]),
         {Nid, N, D, Err}
       end || D <- Deltas],
      cortex:updateWeights(Cortex_Id, [{Nid, WL, Bias}]),
      Choice = lists:sort(fun({_, _, _, Va}, {_, _, _, Vb}) -> (Va < Vb) end, LD),
      io:format(user, "    Impact Step completed for all deltas and N=~p: min={NID, N, Delta, Error} =~256p.~n", [N, hd(Choice)]),
      hd(Choice)
    end || N <- lists:seq(-1, length(WL)-1)] || {Nid, WL, Bias} <- cortex:extractWeightsList(Cortex_Id)],
%  io:format(user, "~nLT := ~128p.~n", [lists:flatten(LT)]),
  [P0, _P1 | _] = lists:sort(fun({_, _, _, Va}, {_, _, _, Vb}) -> (Va < Vb) end, lists:flatten(LT)),
  [case Nw of
     -1 ->
       {_Nid, WTs, Bias} = cortex:extractWeightsList(Cortex_Id, Nid),
       cortex:updateWeights(Cortex_Id, [{Nid, WTs, Bias + Dlt}]);
     _  ->
       {_Nid, WTs, Bias} = cortex:extractWeightsList(Cortex_Id, Nid),
       cortex:updateWeights(Cortex_Id, [{Nid, incNth(Nw, Dlt, WTs), Bias}])
   end || {Nid, Nw, Dlt, _} <- [P0]], %[P0, P1]],
  P0.

run_step_pt(_Cortex_Id, _Impacts, Err, 0) -> {Err, 0};
run_step_pt(Cortex_Id, Impacts, Min_Error, M) ->
  [cortex:perturbWeights(Cortex_Id, Nid) || {Nid, _WL, _Bias} <- cortex:extractWeightsList(Cortex_Id)],
%  io:format(user, "~nLT := ~128p.~n", [lists:flatten(LT)]),
  Err = external_impact(Cortex_Id, Impacts, 0),
  if Err < Min_Error ->
      run_step_pt(Cortex_Id, Impacts, Err, M - 1);
%%      {Err, M};
    true ->
      cortex:rollbackWeights(Cortex_Id),
      run_step_pt(Cortex_Id, Impacts, Min_Error, M - 1)
  end.

incNth(N, Dlt, List) ->
  {New_List, _} = lists:mapfoldl(fun(A, Acc) -> {if (Acc =:= N) -> A + Dlt; true -> A end, Acc + 1} end, 0, List),
  New_List.
%%   incNth(N, Dlt, List, []).
%% 
%% incNth(_N, _Dlt, [], Rslt) ->
%%   lists:reverse(Rslt);
%% incNth(0, Dlt, [H | List], Rslt) ->
%%   incNth(-1, Dlt, List, [(H + Dlt) | Rslt]);
%% incNth(N, Dlt, [H | List], Rslt) ->
%%   incNth(N - 1, Dlt, List, [H | Rslt]).

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


