%% Seminar 6. Distributed Systems course. ICT KTH P1 2015

-module(storage).

-export([add/3,create/0,lookup/2,split/3,merge/2]).

create() ->
  [].

add(Key,Value,Store) ->
  [{Key,Value}|Store].

lookup(Key,Store) ->
  lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
  {Updated, Rest} = lists:foldl(
    fun({Key,Value},{AccSplit1,AccSplit2}) ->
      
      case key:between(Key, To, From) of
          true ->
            {[{Key,Value}|AccSplit1],AccSplit2};
          false ->
            {AccSplit1,[{Key,Value}|AccSplit2]}
      end

    end, {[],[]}, Store).

merge(Entries,Store) ->
  lists:append(Entries,Store).