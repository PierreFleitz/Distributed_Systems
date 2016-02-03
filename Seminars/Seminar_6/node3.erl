%% Seminar 6. Distributed Systems course. ICT KTH P1 2015

-module(node3).
-export([start/1, start/2]).

-define(Stabilize, 100).
-define(Timeout, 1000).

start(Id) -> %% We are the first node in the ring
	start(Id, nil).

start(Id, Peer) -> %% We are connecting to an existing ring
	timer:start(),
	spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
	Predecessor = nil,
	{ok, Successor} = connect(Id, Peer),
	schedule_stabilize(),
	node(Id, Predecessor, Successor, storage:create()).

connect(Id, nil) -> %% Case in wich we are the first node. here we are our own successor
	{ok,{Id,self()}};
connect(_,Peer) -> %% Case in wich we are trying to connect to an existing ring. 
	Qref = make_ref(),
	Peer ! {key, Qref, self()}, %% => we send a key message to the node that have been given
	receive
		{Qref, Skey} ->
			{ok,{Skey,Peer}}
	after ?Timeout -> %% and wait for a reply
		io:format("Time out : no response ~n", [])
	end.

node(Id, Predecessor, Successor, Store) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);
        {notify, New} ->
            {Pred,NewStore} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, NewStore);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store);
        stabilize ->
        	stabilize(Successor),
        	node(Id, Predecessor, Successor, Store);
        probe ->
        	create_probe(Id, Successor),
        	node(Id, Predecessor, Successor, Store);
       	{probe, Id, Nodes, T} ->
       		remove_probe(T, Nodes),
       		node(Id, Predecessor, Successor, Store);
       	{probe, Ref, Nodes, T} ->
       		forward_probe(Ref, T, Nodes, Id, Successor),
       		node(Id, Predecessor, Successor, Store);
       	{add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);
        {handover, Elements} ->
        	Merged = storage:merge(Store, Elements),
        	node(Id, Predecessor, Successor, Merged)
	end.

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
	{Skey, Spid} = Successor,
	case Pred of
		nil -> %% We should inform it about our existence
			Spid ! {notify, {Id, self()}},
			Successor;
		{Id, _} -> %% Pointing back to us => Nothing to do
			Successor;
		{Skey, _} -> %% Pointing to itself => Should notify it about our existence
			Spid ! {notify, {Id, self()}},
			Successor;
		{Xkey, Xpid} -> %% Pointing to another node
			case key:between(Xkey, Id, Skey) of
				true -> %% The key of the predecessor of our succesor (Xkey) is between us and our successor => we should of course adopt this nod as our successor and run stabilization again)
					Xpid ! {request, self()},
					Pred;
				false -> %% Case where we should be in between the nodes => we inform our successor of our existence
					Spid ! {notify, {Id, self()}},
					Successor
			end
	end.

schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).

request(Peer, Predecessor) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil};
		{Pkey, Ppid} ->
			Peer ! {status, {Pkey, Ppid}}
	end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
	case Predecessor of
		nil ->
			Keep = handover(Id, Store, Nkey, Npid),
			{{Nkey, Npid},Keep};
		{Pkey, _} -> 
			case key:between(Nkey, Pkey, Id) of
				true ->
					Keep = handover(Store, Nkey, Npid, Id),
					{{Nkey, Npid},Keep};
				false ->
					{Predecessor,Store}
			end
	end.

handover(Id, Store, Nkey, Npid) ->
	{Keep, Rest} = storage:split(Id, Nkey, Store),
	Npid ! {handover, Rest},
	Keep.

create_probe(Id,{_,Spid}) ->
	Spid ! {probe,Id,[Id],erlang:now()}.

remove_probe(T, Nodes) ->
	Duration = timer:now_diff(erlang:now(),T),
	Printer = fun(E) -> io:format("~p ", [E]) end,
	list:foreach(Printer, Nodes),
	io:format("~n Time = ~p",[Duration]).

forward_probe(Ref, T, Nodes, Id, {_,Spid}) ->
	Spid ! {probe,Ref,Nodes ++ [Id], T}.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
	case key:between(Key, Pkey, Id) of
		true ->
			Client ! {Qref, ok},
			storage:add(Key, Value, Store);
		false ->
			Spid ! {add, Key, Value, Qref, Client},
			Store
	end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
	case key:between(Key, Pkey, Id) of
		true ->
			Result = storage:lookup(Key, Store),
			Client ! {Qref, Result};
		false ->
			{_, Spid} = Successor,
			Spid ! {loojup, Key, Qref, Client}
	end.

