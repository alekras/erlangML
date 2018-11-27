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
  do_setup/1,
  do_cleanup/2
]).
-import(testing, [wait_all/1]).
%%
%% API Functions
%%
neuron_test_() ->
	[ 
		{ setup, 
			fun testing:do_start/0, 
			fun testing:do_stop/1, 
			{inorder, [
				{ foreachx, 
					fun ?MODULE:do_setup/1, 
					fun ?MODULE:do_cleanup/2, 
					[
            {{1, neuron}, fun create/2},
            {{1, sensor}, fun create/2},
            {{1, actuator}, fun create/2},
            {{2, signal}, fun signal/2}
					]
			 }
			]}
		}
	].

do_setup({1, neuron}) ->
  #inp_config{type = neuron, nid = 1, bias = 0.0, input = [#inp_item{nid = 0, weight = 1.0}]};
do_setup({1, sensor}) ->
  #inp_config{type = sensor, nid = 1};
do_setup({1, actuator}) ->
  #inp_config{type = actuator, nid = 1, input = [#inp_item{nid = 0}]};
do_setup({2, signal}) ->
  #inp_config{type = neuron, nid = 10, bias = 0.2, input = [#inp_item{nid = 0, weight = 1.0},
                                                            #inp_item{nid = 1, weight = 1.0},
                                                            #inp_item{nid = 2, weight = 1.0},
                                                            #inp_item{nid = 3, weight = 1.0}
                                                           ]}.

create({1, _Type} = _X, Config) -> {"NN neuron creation test", timeout, 1, fun() ->
%  ?debug_Fmt("~n::test:: neuron create: ~p ~p",[_X, Config]),
  register(test_result, self()),

  {ok, Neuron_Pid} = neuron:neuron(Config),
  neuron:connect(Neuron_Pid, [#out_item{nid = 2, pid = self()}], self()),
  neuron:signal(Neuron_Pid, 0, 1.0),

  receive
    {_, {signal,1, _}} -> 
      ?assert(true);
    {_, {actuator,1, _}} -> 
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

signal({2, _Type} = _X, Config) -> {"NN neuron signal test", timeout, 1, fun() ->
%  ?debug_Fmt("~n::test:: neuron signal: ~p ~p",[_X, Config]),
  register(test_result, self()),

  {ok, Neuron_Pid} = neuron:neuron(Config),
  neuron:connect(Neuron_Pid, [#out_item{nid = 11, pid = self()}], self()),
  neuron:signal(Neuron_Pid, 0, 0.05),
  neuron:signal(Neuron_Pid, 1, 0.05),
  neuron:signal(Neuron_Pid, 2, 0.05),
  neuron:signal(Neuron_Pid, 3, 0.05),

  receive
    {_, {signal, 10, 0.3799489622552249}} -> 
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

do_cleanup(_X, _Y) ->
%  ?debug_Fmt("::test:: teardown after: ~p  ~p",[_X, _Y]),
  ok.
