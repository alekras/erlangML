%% @author axk456
%% @doc @todo Add description to neuron_sup.


-module(neuron_sup).
-behaviour(supervisor).
-include("nn.hrl").
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, build_nn/2, connect_nn/2]).

start_link() ->
  supervisor:start_link(?MODULE, []).

build_nn(Genotype, Neuron_Sup_Pid) ->
  Neuron_Child_Spec = [{NId, {neuron, neuron, [Config]}, permanent, 2000, worker, [neuron]} || #inp_config{nid = NId} = Config <- Genotype],
  [{Ch_NId, Ch_Pid} || {Ch_NId, {ok, Ch_Pid}} <- [{NId, supervisor:start_child(Neuron_Sup_Pid, Ch_Spec)} || {NId, _, _, _, _, _} = Ch_Spec <- Neuron_Child_Spec]].

connect_nn(Neuron_Output_List, Cortex_Pid) ->
  [neuron:connect(Pid, OutList, Cortex_Pid) || #out_config{pid = Pid, output = OutList} <- Neuron_Output_List].

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
init([]) ->
    {ok,{{one_for_one,1,1}, []}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


