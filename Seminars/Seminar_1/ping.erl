-module(ping).
-export([ping/0]).

ping() -> 
	{ pong, Pid } ! { ping, self() }. %{pong, self()} ?

pong() ->
	receive 
		X -> io:format("Pong a recu le pid de ping() : ~p~n", [X])
	end.