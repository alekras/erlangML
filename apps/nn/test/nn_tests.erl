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
						{{1, config}, fun configure/2}
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
  Fun1 = fun(R) -> ?debug_Fmt("::test:: RESULT[0]: ~128p.", [R]), test_result ! done end,
  cortex:set_call_back(cortex_1, Fun1),

  cortex_sup:new_nn(cortex_sup, 2),
  cortex:applyGenotype(cortex_2, configuration(1)),
  Fun2 = fun(R) -> ?debug_Fmt("::test:: RESULT[1]: ~128p.", [R]), test_result ! done end,
  cortex:set_call_back(cortex_2, Fun2),

%  cortex:send_signal_to(cortex_1, [{0, 2}, {6, 1}]),
%  wait_all(1),
  cortex:send_signal_to(cortex_2, [{0, 2.0}, {1, 1.0}]),
  W2 = wait_all(1),
  cortex:updateWeights(cortex_2, [{7, [0.5,3.0,1.3]}]),
  cortex:send_signal_to(cortex_2, [{0, 2.0}, {1, 1.0}]),
  W3 = wait_all(1),
%%   cortex:send_signal_to(cortex_1, [{0, 0.5}, {6, 3}]),
%% 	W4 = wait_all(1),
	unregister(test_result),
	?assert(W3),
%%  ?assert(W2),
  ?PASSED
end}.

configuration(0) ->
  [
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
    #inp_config{type = neuron, nid = 2, input = [#inp_item{nid = 0, weight = 0.5}],
                bias = 0.5},
    #inp_config{type = neuron, nid = 3, input = [#inp_item{nid = 0, weight = 1.5},
                                                 #inp_item{nid = 1, weight = 0.5}],
                bias = 0.4},
    #inp_config{type = neuron, nid = 4, input = [#inp_item{nid = 1, weight = 0.9}],
                bias = 0.5},
    #inp_config{type = neuron, nid = 5, input = [#inp_item{nid = 2, weight = 1.2},
                                                 #inp_item{nid = 3, weight = 0.9},
                                                 #inp_item{nid = 4, weight = 0.5},
                                                 #inp_item{nid = 6, weight = 0.5}
                                                ],
                bias = 0.5},
    #inp_config{type = neuron, nid = 6, input = [#inp_item{nid = 3, weight = 1.5},
                                                 #inp_item{nid = 4, weight = 0.5}],
                bias = 0.4},
    #inp_config{type = neuron, nid = 7, input = [#inp_item{nid = 3, weight = 1.5},
                                                 #inp_item{nid = 5, weight = 0.5},
                                                 #inp_item{nid = 6, weight = 0.5}
                                                ],
                bias = 0.4},
    #inp_config{type = actuator, nid = 8, input = [#inp_item{nid = 2},
                                                   #inp_item{nid = 7}
                                                  ]},
    #inp_config{type = actuator, nid = 9, input = [#inp_item{nid = 6},
                                                   #inp_item{nid = 7}
                                                  ]}
  ].
