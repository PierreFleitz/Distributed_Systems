-module(vect).
-export([zero/0,inc/2,merge/2,leq/2,clock/1,update/3,safe/2]).

zero() -> 
	[].

inc(Name, T) ->
	case lists:keyfind(Name, 1, T) of
		{Name, V} -> 
			lists:keyreplace(Name, 1, T, {Name, V+1});
		false ->
			[{Name, 1}|T]
	end.

merge([],Time) ->
	[];
merge([{Name, Ti}|Rest], Time) ->
	case lists:keyfind(Name, 1, Time) of
		{Name, Tj} ->
			[{Name, max(Ti,Tj)}|merge(Rest, lists:keydelete(Name,1,Time))];
		false ->
			[{Name,Ti}|merge(Rest,Time)]
	end.

leq([],_) ->
	true;
leq([{Name,Ti}|Rest], Time) ->
	case lists:keyfind(Name,1,Time) of
		{Name,Tj} ->
			if
				Ti =< Tj ->
					leq(Rest,Time);
				true ->
					false
			end;
		false ->
			false
	end.

clock(Nodes) ->
	list:foldl(fun(Node, S) -> [{Node, zero()}|S] end, [], Nodes).

update(Node, Time, Clock) ->
	lists:keyreplace(Node,1,Clock,{Node,Time}).

safe(Time, Clock) ->
	lists:all(fun({_,Time2}) -> leq(Time,Time2) end, Clock).