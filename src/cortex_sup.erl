%% @author alekras
%% @doc @todo Add description to cortex.


-module(cortex_sup).
-behaviour(supervisor).
-include("nn.hrl").
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).


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
  Configuration = [
    #config{type = sensor, nid = 0, input = [], bias = undefined},
    #config{type = neuron, nid = 1, input = [#inp_item{nid = 0, weight = 1.2}], bias = 0.3},
    #config{type = neuron, nid = 2, input = [#inp_item{nid = 1, weight = 0.5}], bias = 0.4},
    #config{type = neuron, nid = 3, input = [#inp_item{nid = 0, weight = 1.0}, #inp_item{nid = 1, weight = 0.9}], bias = 0.5},
    #config{type = actuator, nid = 4, input = [#inp_item{nid = 0, weight = 1.2}], bias = undefined}
  ],
  
  Childs = [{NId, {neuron, Type, [Config]}, permanent,2000,worker,[neuron]} || #config{type = Type, nid = NId} = Config <- Configuration],
%%  AChild = {'AName',{'AModule',start_link,[]}, permanent,2000,worker,['AModule']},
  {ok,{{one_for_one, 1, 1}, Childs}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
info() ->
  supervisor:which_children(cortex_sup)
.

