%% Seminar 6. Distributed Systems course. ICT KTH P1 2015

-module(node1).
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
	node(Id, Predecessor, Successor).

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

node(Id, Predecessor, Successor) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        stabilize ->
        	stabilize(Successor),
        	node(Id, Predecessor, Successor);
        probe ->
        	create_probe(Id, Successor),
        	node(Id, Predecessor, Successor);
       	{probe, Id, Nodes, T} ->
       		remove_probe(T, Nodes),
       		node(Id, Predecessor, Successor);
       	{probe, Ref, Nodes, T} ->
       		forward_probe(Ref, T, Nodes, Id, Successor),
       		node(Id, Predecessor, Successor)
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

notify({Nkey, Npid}, Id, Predecessor) ->
	case Predecessor of
		nil -> %% Our own predecessor is set to the nil, case closed
			{Nkey, Npid};
		{Pkey, _} -> %% We already have a predecessor :
			case key:between(Nkey, Pkey, Id) of
				true -> %% => We have to check if the new node actually should be our predecessor
					{Nkey, Npid};
				false -> %% => Or not.
					Predecessor
			end
	end.

create_probe(Id,{_,Spid}) ->
	Spid ! {probe,Id,[Id],erlang:now()}.

remove_probe(T, Nodes) ->
	Duration = timer:now_diff(erlang:now(),T),
	Printer = fun(E) -> io:format("~p ", [E]) end,
	list:foreach(Printer, Nodes),
	io:format("~n Time = ~p",[Duration]).

forward_probe(Ref, T, Nodes, Id, {_,Spid}) ->
	Spid ! {probe,Ref,Nodes ++ [Id], T}.