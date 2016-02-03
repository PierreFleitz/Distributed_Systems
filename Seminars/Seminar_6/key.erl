%% Seminar 6. Distributed Systems course. ICT KTH P1 2015

-module(key).
-export([generate/0, between/3]).

generate() ->
	random:uniform(1000000000).

between(Key, From, To) ->
	if
		From<To ->
			(Key>From) and (Key=<To);
		From>To ->
			(Key>From) or (Key=<To);
		true ->
			true
	end.