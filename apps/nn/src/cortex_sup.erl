%% @author alekras
%% @doc @todo Add description to cortex.


-module(cortex_sup).
-behaviour(supervisor).
-include("nn.hrl").
-export([init/1, handle_info/2]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, new_nn/2]).

start_link() ->
  supervisor:start_link({local, cortex_sup}, ?MODULE, 0).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init(NN_ID) ->
  Cortex_Id = list_to_atom(lists:concat(["cortex_", NN_ID])),
  io:format(user, "cortex_sup init: ~p ~p ~n", [Cortex_Id, NN_ID]),
%%  Childs = [{Cortex_Id, {cortex, start_link, [NN_ID]}, permanent, 2000, worker, [cortex]}],
  Childs = [],
%%  | [{NId, {neuron, Type, [Config]}, permanent, 2000, worker, [neuron]} || #inp_config{type = Type, nid = NId} = Config <- configuration()]],
%%  AChild = {'AName',{'AModule',start_link,[]}, permanent,2000,worker,['AModule']},
  {ok,{{one_for_one, 1, 1}, Childs}}.

new_nn(Sup_Pid, N) ->
  Cortex_Id = list_to_atom(lists:concat(["cortex_", N])),
  io:format(user, "cortex_sup new: ~p ~p ~n", [Cortex_Id, N]),
  Ch_Spec = {Cortex_Id, {cortex, start_link, [N]}, permanent, 2000, worker, [cortex]},
  supervisor:start_child(Sup_Pid, Ch_Spec).

%% ====================================================================
%% Internal functions
%% ====================================================================
handle_info(_Info, State) ->
  io:format(user, "handle_info: ~128p ~128p ~n", [_Info, State]),
  {noreply, State}.



