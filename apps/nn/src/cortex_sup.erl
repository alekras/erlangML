%% @author alekras
%% @doc @todo Add description to cortex.


-module(cortex_sup).
-behaviour(supervisor).
-include("nn.hrl").
-export([init/1, handle_info/2]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([configuration/0, start_link/0, send_signal_to/2]).

configuration() ->
  [
    #inp_config{type = sensor, nid = 0, input = [], bias = undefined},
    #inp_config{type = neuron, nid = 1, input = [#inp_item{nid = 0, weight = 1.2}], bias = 0.3},
    #inp_config{type = neuron, nid = 2, input = [#inp_item{nid = 1, weight = 0.5}], bias = 0.4},
    #inp_config{type = neuron, nid = 3, input = [#inp_item{nid = 0, weight = 1.0}, #inp_item{nid = 1, weight = 0.9}], bias = 0.5},
    #inp_config{type = neuron, nid = 4, input = [#inp_item{nid = 1, weight = 1.2}, #inp_item{nid = 2, weight = 0.9}, #inp_item{nid = 3, weight = 0.5}], bias = 0.5},
    #inp_config{type = actuator, nid = 5, input = [#inp_item{nid = 2, weight = 1}, #inp_item{nid = 3, weight = 1}, #inp_item{nid = 4, weight = 1}], bias = undefined}
  ].

start_link() ->
  supervisor:start_link({local, cortex_sup}, ?MODULE, [1]).

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
  Childs = [{Cortex_Id, {cortex, start_link, [NN_ID]}, permanent, 2000, worker, [cortex]}],
%%  | [{NId, {neuron, Type, [Config]}, permanent, 2000, worker, [neuron]} || #inp_config{type = Type, nid = NId} = Config <- configuration()]],
%%  AChild = {'AName',{'AModule',start_link,[]}, permanent,2000,worker,['AModule']},
  {ok,{{one_for_one, 1, 1}, Childs}}.

send_signal_to(Nid, Val) ->
  Id_Pid = [{Id, Pid} || {Id, Pid, _, _} <- supervisor:which_children(cortex_sup)],
  Pid = proplists:get_value(Nid, Id_Pid),
  neuron:signal(Pid, 0, Val).
%% ====================================================================
%% Internal functions
%% ====================================================================
handle_info(_Info, State) ->
  io:format(user, "handle_info: ~128p ~128p ~n", [_Info, State]),
  {noreply, State}.



