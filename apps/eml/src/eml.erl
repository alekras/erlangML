%% @author axk456
%% @doc @todo Add description to eml.


-module(eml).
-behaviour(application).
-export([start/2, stop/1, env/0]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
	{ok, Pid :: pid()}
	| {ok, Pid :: pid(), State :: term()}
	| {error, Reason :: term()}.
%% ====================================================================
start(Type, StartArgs) ->
  io:format(user, "Application started. Type=~p~n", [Type]),
  case cortex_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

env() ->
  loop().
%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
stop(_State) ->
  ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
loop() ->
%%   R0 = application:stop(eml),
%%   io:format(user, "After app stop.   R0=~p~n", [R0]),
  R1 = application:unload(dynconf),
  io:format(user, "After app unload. R1=~p~n", [R1]),
  R2 = application:load(dynconf),
  io:format(user, "After app load.   R2=~p~n", [R2]),
%%   R3 = application:start(eml),
%%   io:format(user, "After app start.  R3=~p~n", [R3]),
  Test = application:get_env(dynconf, test),
  io:format(user, "After config changed. Test=~p~n~n", [Test]),
  timer:sleep(2000),
  loop().


