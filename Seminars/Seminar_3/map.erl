-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
	[].

update(Node,Links,Map) ->
	Tmp = lists:keydelete(Node, 1, Map),
	[{Node,Links} | Tmp].

reachable(Node,Map) ->
	case lists:keyfind(Node, 1, Map) of
			{_,Links} ->
				Links;
			false ->
				[]
	end.

all_nodes(Map) ->
	FlattenedMap = lists:flatmap(fun({Node,Links}) -> [Node|Links] end, Map),
	lists:foldl(fun(Elem,Acc) ->
				case {Elem,Acc} of
					{Elem,[Elem|_]} ->
						Acc;
					_ ->
						[Elem|Acc]
				end
			end,
		[], lists:sort(FlattenedMap)).