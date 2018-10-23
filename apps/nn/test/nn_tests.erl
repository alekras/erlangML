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
  ?debug_Fmt("~n::test:: configure: ~p ~p",[_X, _Y]),
  cortex:applyGenotype(cortex_1, configuration()),
  cortex:send_signal_to(cortex_1, 0, 2),
  ?debug_Fmt("::test:: RESULT: ~128p.", [cortex:result(cortex_1)]),
%  cortex:send_signal_to(cortex_1, 0, 2),
  ?PASSED
end}.

configuration() ->
  [
    #inp_config{type = sensor, nid = 0, input = [], bias = undefined},
    #inp_config{type = neuron, nid = 1, input = [#inp_item{nid = 0, weight = 1.2}], bias = 0.3},
    #inp_config{type = neuron, nid = 2, input = [#inp_item{nid = 1, weight = 0.5}], bias = 0.4},
    #inp_config{type = neuron, nid = 3, input = [#inp_item{nid = 0, weight = 1.0}, #inp_item{nid = 1, weight = 0.9}], bias = 0.5},
    #inp_config{type = neuron, nid = 4, input = [#inp_item{nid = 1, weight = 1.2}, #inp_item{nid = 2, weight = 0.9}, #inp_item{nid = 3, weight = 0.5}], bias = 0.5},
    #inp_config{type = actuator, nid = 5, input = [#inp_item{nid = 2, weight = 1}, #inp_item{nid = 3, weight = 1}, #inp_item{nid = 4, weight = 1}], bias = undefined}
  ].
