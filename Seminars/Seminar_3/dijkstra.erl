-module(dijkstra).
%%-export([entry/2, replace/4, update/4, iterate/3, table/2, route/2]).
-export([table/2, route/2]). %% Ce sont les seuls procedures que l'on doit exporter

entry(Node,Sorted) ->
	case lists:keyfind(Node, 1, Sorted) of
		{_,L,_} -> L;
		false -> 0
	end.

replace(Node,N,Gateway,Sorted) ->
	Tmp = lists:keyreplace(Node, 1, Sorted, {Node,N,Gateway}),
	lists:keysort(2,Tmp). %% We do the sorting on the Nth (here second) elmt of the tuple, so here N

update(Node,N,Gateway,Sorted) ->
	L = entry(Node, Sorted),
	case (N < L) of
		true ->
			replace(Node, N, Gateway, Sorted);
		false ->
			Sorted
	end.

iterate(Sorted,Map,Table) ->
	case Sorted of
			[] ->
				Table;
			[{_,inf,_}|_] ->
				Table;
			[Entry|T] ->
				{Dest,L,Gateway} = Entry,
				case lists:keyfind(Dest,1,Map) of
					{_,Reachables} ->
						NewSortedList = lists:foldl(fun(Elem,SortedList) ->
											update(Elem,L+1,Gateway,SortedList)
											end, T, Reachables);
					false ->
						NewSortedList = T
				end,
				iterate(NewSortedList,Map,[{Dest,Gateway} | Table])
			end.

table(Gateways,Map) ->
	AllNodes = map:all_nodes(Map), %% retourne une liste de toutes les nodes
	FirstSortedList = lists:keysort(2, lists:map(fun(Elem) -> %% Ici on va appliquer la fonction suivante sur tous les éléments de la liste de nodes que l'on va trier on retourne cette liste triée à la fin
											case lists:member(Elem, Gateways) of
												true ->
													{Elem,0,Elem};
												false ->
													{Elem,inf,unknown}
												end
											end, AllNodes)),
	iterate(FirstSortedList,Map,[]).

route(Node, Table) ->
	case lists:keyfind(Node,1,Table) of
		{Dest,Gateway} ->
			{ok,Gateway};
		false ->
			notfound
	end.