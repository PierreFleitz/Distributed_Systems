-module(time).
-export([zero/0,inc/2,merge/2,leq/2,clock/1,update/3,safe/2]).

%zero() -> 0.

zero() -> [].

inc(Name, T) ->
	T + 1.

merge(Ti,Tj) ->
	erlang:max(Ti,Tj).

leq(Ti,Tj) ->
	if 
		Ti =< Tj ->
			true
	end.

clock(Nodes) ->
	list:foldl(fun(Node, S) -> [{Node, zero()}|S}] end, [], Nodes).

update(Node, Time, Clock) ->
	list:keyreplace(Node,1,Clock,(Node,Time)).

safe(Time, Clock) ->
	list:all(fun({_,Time2}) -> leq(Time,Time2) end, Clock).
