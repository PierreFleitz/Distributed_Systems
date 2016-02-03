-module(ping).
-export([ping/1]).

ping() -> Pid ! {self(), pong}. %{pong, self()} ?

pong() ->
	receive 
		X -> io:format("Pong a recu le pid de ping() : ~p~n", [X])
	end.