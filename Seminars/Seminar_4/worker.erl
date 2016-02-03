-module(worker).
-export([start/5,stop/1,peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
	spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
	Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
	random:seed(Seed, Seed, Seed),
	receive
		{peers, Peers} ->
			loop(Name, Log, Peers, Sleep, Jitter, vect:zero());
		stop ->
			ok
	end.

peers(Wrk,Peers) ->
	Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Counter) ->
	Wait = random:uniform(Sleep), %% If Sleep = 4 random;uniform(Sleep) will return a random interger unformly distributed between 1 and 4
	receive
		{msg,Time,Msg} ->
			NewTime = vect:merge(Counter,Time)+1,
			Log ! {log, Name, NewTime, {received, Msg}},
			loop(Name, Log, Peers, Sleep, Jitter, NewTime);
		stop ->
			ok;
		Error ->
			Log ! {log, Name, time, {error, Error}}
	after Wait ->
		Selected = select(Peers),
		Time = Counter + 1,
		Message = {hello, random:uniform(100)},
		Selected ! {msg, Time, Message},
		jitter(Jitter),
		Log ! {log, Name, Time, {sending, Message}},
		loop(Name, Log, Peers, Sleep, Jitter, Time)
	end.

select(Peers) ->
	lists:nth(random:uniform(length(Peers)), Peers). %% lists:nth = Returns the Nth element of List so here it will return a random element of the list Peers (we choose a random peer to send the message)

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
