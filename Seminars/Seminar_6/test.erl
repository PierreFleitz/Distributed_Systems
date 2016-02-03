%% Seminar 6. Distributed Systems course. ICT KTH P1 2015

-module(test).

-compile(export_all).

-define(Timeout, 1000).


%% Starting up a set of nodes is made easier using this function.

start(Module) ->
    Id = key:generate(), 
    apply(Module, start, [Id]).


start(Module, P) ->
    Id = key:generate(), 
    apply(Module, start, [Id,P]).    

start(_, 0, _) ->
    ok;
start(Module, N, P) ->
    start(Module, P),
    start(Module, N-1, P).

%% The functions add and lookup can be used to test if a DHT works.

add(Key, Value , P) ->
    Q = make_ref(),
    P ! {add, Key, Value, Q, self()},
    receive 
	{Q, ok} ->
	   ok
	after ?Timeout ->
	    {error, "timeout"}
    end.

lookup(Key, Node) ->
    Q = make_ref(),
    Node ! {lookup, Key, Q, self()},
    receive 
	{Q, Value} ->
	    Value
    after ?Timeout ->
	    {error, "timeout"}
    end.


%% This benchmark can be used for a DHT where we can add and lookup
%% key. In order to use it you need to implement a store.

keys(N) ->
    lists:map(fun(_) -> key:generate() end, lists:seq(1,N)).

add(Keys, P) ->
    lists:foreach(fun(K) -> add(K, gurka, P) end, Keys).

check(Keys, P) ->
    T1 = now(),
    {Failed, Timeout} = check(Keys, P, 0, 0),
    T2 = now(),
    Done = (timer:now_diff(T2, T1) div 1000),
    io:format("~w lookup operation in ~w ms ~n", [length(Keys), Done]),
    io:format("~w lookups failed, ~w caused a timeout ~n", [Failed, Timeout]).


check([], _, Failed, Timeout) ->
    {Failed, Timeout};
check([Key|Keys], P, Failed, Timeout) ->
    case lookup(Key,P) of
	{Key, _} -> 
	    check(Keys, P, Failed, Timeout);
	{error, _} -> 
	    check(Keys, P, Failed, Timeout+1);
	false ->
	    check(Keys, P, Failed+1, Timeout)
    end.

%% TEST PROCEDURE %% 
test() ->

    Node = node2:start(key:generate()),
    %Node2 = node2:start(key:generate(), Node),
    %Node3 = node2:start(key:generate(), Node),
    %Node4 = node2:start(key:generate(), Node),
    timer:sleep(1000),
    Test_machines = lists:seq(1, 4),
    lists:foreach(fun(_) -> spawn(fun() -> add_lookup(Node, 1000) end) end, Test_machines).
    %spawn(fun() -> add_lookup(Node,1000) end),
    %spawn(fun() -> add_lookup(Node,1000) end),
    %spawn(fun() -> add_lookup(Node,1000) end),
    %spawn(fun() -> add_lookup(Node,1000) end).

add_lookup(Node,Nb) ->
    Tuples = [{key:generate(),N} || N <- lists:seq(1, Nb)],
    lists:foreach(fun({Key,Value}) ->
        Node ! {add, Key, Value, Key, self()}
        end,Tuples),

    T1 = erlang:now(),    
    %% looking up 
    lists:foreach(fun({Key,Value}) ->
        Node ! {lookup, Key, Key, self()}
        end, Tuples),

    T2 = erlang:now(),
    Duration = timer:now_diff(T2, T1)/1000000,
    io:format("Duration : ~p~n",[Duration]).




