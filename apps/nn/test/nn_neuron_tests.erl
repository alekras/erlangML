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

-module(nn_neuron_tests).

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
            {{1, neuron}, fun create/2},
            {{1, sensor}, fun create/2},
            {{1, actuator}, fun create/2}
					]
			 }
			]}
		}
	].

create({1, neuron} = _X, _Y) -> {"NN neuron creation test", timeout, 1, fun() ->
  ?debug_Fmt("~n::test:: neuron create: ~p ~p",[_X, _Y]),
  register(test_result, self()),

  {ok, Neuron_Pid} = neuron:neuron(#inp_config{type = neuron, nid = 1, bias = 0.0, input = [#inp_item{nid = 0, weight = 1.0}]}),
  neuron:connect(Neuron_Pid, [#out_item{nid = 2, pid = self()}], self()),
  neuron:signal(Neuron_Pid, 0, 1.0),
  
  receive
    {_, {signal,1, _}} -> 
      ?assert(true);
    Msg ->
      ?debug_Fmt("::test:: Msg came: ~p",[Msg]),
      ?assert(false)
  end,
%  W1 = wait_all(1),

  unregister(test_result),
%  ?assert(W1),
  ?PASSED
end};

create({1, sensor} = _X, _Y) -> {"NN sensor creation test", timeout, 1, fun() ->
  ?debug_Fmt("~n::test:: sensor create: ~p ~p",[_X, _Y]),
  register(test_result, self()),

  {ok, Neuron_Pid} = neuron:sensor(#inp_config{type = sensor, nid = 0}),
  neuron:connect(Neuron_Pid, [#out_item{nid = 1, pid = self()}], self()),
  neuron:signal(Neuron_Pid, -1, 1.0),

  receive
    {_, {signal, 0, _}} -> 
      ?assert(true);
    Msg ->
      ?debug_Fmt("::test:: Msg came: ~p",[Msg]),
      ?assert(false)
  end,
%  W1 = wait_all(1),

  unregister(test_result),
%  ?assert(W1),
  ?PASSED
end};

create({1, actuator} = _X, _Y) -> {"NN sensor actuator test", timeout, 1, fun() ->
  ?debug_Fmt("~n::test:: actuator create: ~p ~p",[_X, _Y]),
  register(test_result, self()),

  {ok, Neuron_Pid} = neuron:actuator(#inp_config{type = actuator, nid = 1, input = [#inp_item{nid = 0}]}),
  neuron:connect(Neuron_Pid, [], self()),
  neuron:signal(Neuron_Pid, 0, 1.0),

  receive
    {_, {actuator, 1, _}} -> 
      ?assert(true);
    Msg ->
      ?debug_Fmt("::test:: Msg came: ~p",[Msg]),
      ?assert(false)
  end,
%  W1 = wait_all(1),

  unregister(test_result),
%  ?assert(W1),
  ?PASSED
end}.
