-module(hist).
-export([new/1,update/3]).

new(Name) -> [{Name, inf}].

update(Node, N, History) ->

	case lists:keyfind(Node, 1, History) of
    	{Node, Counter} ->
    		if
        		N > Counter ->
        			{new, [{Node, N} | lists:keyrdelete(Node, 1, History)]};
        		true -> old
    		end;
    	false ->
    		{new, [{Node,N} | History]}
	end.