%%
%% Copyright (C) 2018-2018 by krasnop@bellsouth.net (Alexei Krasnopolski)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%		 http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License. 
%%

%% @hidden
%% @since 2018-10-10
%% @copyright 2018-2018 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module is running erlang unit tests.

-module(nn_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("nn.hrl").
-include("test.hrl").

-export([
]).
-import(testing, [wait_all/1]).
%%
%% API Functions
%%
eml_test_() ->
	[ 
		{ setup, 
			fun testing:do_start/0, 
			fun testing:do_stop/1, 
			{inorder, [
				{ foreachx, 
					fun testing:do_setup/1, 
					fun testing:do_cleanup/2, 
					[
%						{{1, config}, fun configure/2}
            {{1, train}, fun train/2}
					]
			 }
			]}
		}
	].

configure(_X, _Y) -> {"NN configuration test", timeout, 15, fun() ->
  register(test_result, self()),

  ?debug_Fmt("~n::test:: configure: ~p ~p",[_X, _Y]),
  cortex_sup:new_nn(cortex_sup, 1),
  cortex:applyGenotype(cortex_1, configuration(0)),
  Fun1 = fun(R) -> ?debug_Fmt("::test:: RESULT[1]: ~128p.", [R]), test_result ! done end,
  cortex:set_call_back(cortex_1, Fun1),

  cortex_sup:new_nn(cortex_sup, 2),
  cortex:applyGenotype(cortex_2, configuration(1)),
  Fun2 = fun(R) -> ?debug_Fmt("::test:: RESULT[2]: ~128p.", [R]), test_result ! done end,
  cortex:set_call_back(cortex_2, Fun2),

%  cortex:send_signal_to(cortex_1, [{0, 2}, {6, 1}]),
%  wait_all(1),
  cortex:send_signal_to(cortex_2, [{0, 2.0}, {1, 1.0}]),
  W2 = wait_all(1),
  ?debug_Fmt("::test::  weight List: = ~p~n", [cortex:extractWeightsList(cortex_2)]),
  cortex:updateWeights(cortex_2, [{7, [0.5,3.0,1.3]}, {2, [0.9]}]),
  cortex:send_signal_to(cortex_2, [{0, 2.0}, {1, 1.0}]),
  W3 = wait_all(1),
  NewGT = cortex:extractGenotype(cortex_2),
  ?debug_Fmt("::test::  New Genotype = ~p~n", [NewGT]),
  cortex_sup:new_nn(cortex_sup, 3),
  cortex:applyGenotype(cortex_3, NewGT),
  Fun3 = fun(R) -> ?debug_Fmt("::test:: RESULT[3]: ~128p.", [R]), test_result ! done end,
  cortex:set_call_back(cortex_3, Fun3),
  cortex:send_signal_to(cortex_3, [{0, 2.0}, {1, 1.0}]),
  W4 = wait_all(1),
%%   cortex:send_signal_to(cortex_1, [{0, 0.5}, {6, 3}]),
%% 	W4 = wait_all(1),
  unregister(test_result),
  ?assert(W3 and W2 and W4),
%%  ?assert(W2),
  ?PASSED
end}.

train(_X, _Y) -> {"NN train test", timeout, 15, fun() ->
%  ?debug_Fmt("~n::test:: train: ~p ~p",[_X, _Y]),
  register(test_result, self()),

  cortex_sup:new_nn(cortex_sup, 1),
  cortex:applyGenotype(cortex_1, configuration(2)),

  ?debug_Fmt("Weights before Train: ~128p.~n", [cortex:extractWeightsList(cortex_1)]),
  LR = nn_trainer:run_loop(cortex_1, [{[{0, 1.0}, {1, 1.0}], 2.0}, {[{0, 1.0}, {1, 0.0}], 1.0}, {[{0, 0.0}, {1, 1.0}], 1.0}, {[{0, 0.0}, {1, 0.0}], 0.0}], 
                           [-0.8, -0.4, -0.2, 0, 0.2, 0.4, 0.8], 
                           0.01, 50),
  ?debug_Fmt("Train Result: ~128p.", [LR]),
  ?debug_Fmt("Weights after Train: ~128p.", [cortex:extractWeightsList(cortex_1)]),
%  cortex:send_signal_to(cortex_2, [{0, 2.0}, {1, 1.0}]),
  Fun1 = 
    fun(Res) -> 
      ?debug_Fmt("::test:: RESULT[0]: ~128p.", [lists:sum(lists:flatten(Res))]), 
      test_result ! done 
    end,
  cortex:set_call_back(cortex_1, Fun1),
  cortex:send_signal_to(cortex_1, [{0, 1.0}, {1, 1.0}]),
  wait_all(1),
  cortex:send_signal_to(cortex_1, [{0, 1.0}, {1, 0.0}]),
  wait_all(1),
  cortex:send_signal_to(cortex_1, [{0, 0.0}, {1, 1.0}]),
  wait_all(1),
  cortex:send_signal_to(cortex_1, [{0, 0.0}, {1, 0.0}]),
  wait_all(1),
%%  W1 = wait_all(20 * 13),
  unregister(test_result),
%%  ?assert(W1),
  ?PASSED
end}.

configuration(0) -> [
    #inp_config{type = sensor, nid = 0, input = []},
    #inp_config{type = sensor, nid = 6, input = []},
    #inp_config{type = neuron, nid = 1, input = [#inp_item{nid = 0, weight = 1.2}],
                bias = 0.3},
    #inp_config{type = neuron, nid = 2, input = [#inp_item{nid = 1, weight = 0.5}],
                bias = 0.4},
    #inp_config{type = neuron, nid = 3, input = [#inp_item{nid = 0, weight = 1.0},
                                                 #inp_item{nid = 1, weight = 0.9},
                                                 #inp_item{nid = 6, weight = 0.5}
                                                ],
                bias = 0.5},
    #inp_config{type = neuron, nid = 4, input = [#inp_item{nid = 1, weight = 1.2},
                                                 #inp_item{nid = 2, weight = 0.9},
                                                 #inp_item{nid = 6, weight = 0.5},
                                                 #inp_item{nid = 3, weight = 0.5}
                                                ],
                bias = 0.5},
    #inp_config{type = actuator, nid = 5, input = [#inp_item{nid = 2},
                                                   #inp_item{nid = 3},
                                                   #inp_item{nid = 4}
                                                  ]}
];
configuration(1) ->
  [
    #inp_config{type = sensor, nid = 0, input = []},
    #inp_config{type = sensor, nid = 1, input = []},
    #inp_config{type = neuron, nid = 2, input = [#inp_item{nid = 0, weight = 1.5}],
                bias = 0.1},
    #inp_config{type = neuron, nid = 3, input = [#inp_item{nid = 0, weight = 0.5},
                                                 #inp_item{nid = 1, weight = 0.5}],
                bias = 0.02},
    #inp_config{type = neuron, nid = 4, input = [#inp_item{nid = 1, weight = 0.45}],
                bias = 0.15},
    #inp_config{type = neuron, nid = 5, input = [#inp_item{nid = 2, weight = 0.2},
                                                 #inp_item{nid = 3, weight = 0.34},
                                                 #inp_item{nid = 4, weight = 0.25},
                                                 #inp_item{nid = 6, weight = 0.35}
                                                ],
                bias = 0.12},
    #inp_config{type = neuron, nid = 6, input = [#inp_item{nid = 3, weight = 0.15},
                                                 #inp_item{nid = 4, weight = 0.05}],
                bias = 0.214},
    #inp_config{type = neuron, nid = 7, input = [#inp_item{nid = 3, weight = 0.15},
                                                 #inp_item{nid = 5, weight = -0.25},
                                                 #inp_item{nid = 6, weight = 0.45}
                                                ],
                bias = 0.145},
    #inp_config{type = actuator, nid = 8, input = [#inp_item{nid = 5},
                                                   #inp_item{nid = 7}
                                                  ]},
    #inp_config{type = actuator, nid = 9, input = [#inp_item{nid = 6},
                                                   #inp_item{nid = 7}
                                                  ]}
  ];
configuration(2) ->
  W = 0.5,
  B = 0.0,
  [
    #inp_config{type = sensor, nid = 0, input = []},
    #inp_config{type = sensor, nid = 1, input = []},
    #inp_config{type = neuron, nid = 2, input = [#inp_item{nid = 0, weight = W},
                                                 #inp_item{nid = 3, weight = W}],
                bias = B},
    #inp_config{type = neuron, nid = 3, input = [#inp_item{nid = 0, weight = W},
                                                 #inp_item{nid = 1, weight = W}],
                bias = B},
    #inp_config{type = neuron, nid = 4, input = [#inp_item{nid = 0, weight = W},
                                                 #inp_item{nid = 1, weight = W}],
                bias = B},
    #inp_config{type = neuron, nid = 5, input = [#inp_item{nid = 4, weight = W},
                                                 #inp_item{nid = 1, weight = W}],
                bias = B},
    #inp_config{type = neuron, nid = 6, input = [#inp_item{nid = 2, weight = W},
                                                 #inp_item{nid = 3, weight = W}],
                bias = B},
    #inp_config{type = neuron, nid = 7, input = [#inp_item{nid = 3, weight = W},
                                                 #inp_item{nid = 4, weight = W}],
                bias = B},
    #inp_config{type = neuron, nid = 8, input = [#inp_item{nid = 4, weight = W},
                                                 #inp_item{nid = 5, weight = W}],
                bias = B},
    #inp_config{type = neuron, nid = 9, input = [#inp_item{nid = 6, weight = W},
                                                 #inp_item{nid = 7, weight = W}],
                bias = B},
    #inp_config{type = neuron, nid = 10, input = [#inp_item{nid = 7, weight = W},
                                                  #inp_item{nid = 8, weight = W}],
                bias = B},
    #inp_config{type = actuator, nid = 11, input = [#inp_item{nid = 6},
                                                    #inp_item{nid = 9},
                                                    #inp_item{nid = 10}
                                                   ]},
    #inp_config{type = actuator, nid = 12, input = [#inp_item{nid = 8},
                                                    #inp_item{nid = 9},
                                                    #inp_item{nid = 10}
                                                   ]}
  ].
