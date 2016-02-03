-module(http).
-export([parse_request/1, ok/1, get/1]).

parse_request(R0) ->
	{Request, R1} = request_line(R0),
	{Headers, R2} = headers(R1),
	{Body, _} = message_body(R2),
	{Request, Headers, Body}.

request_line([$G, $E, $T, 32 |R0]) -> %%32 = space
	{URI, R1} = request_uri(R0),
	{Ver, R2} = http_version(R1),
	[13,10 |R3] = R2, %% 13 = retour à la ligne ; 10 = nouvelle ligne
	{{get, URI, Ver}, R3}.

request_uri([32 |R0]) ->
	{[], R0};
request_uri([C |R0]) ->
	{Rest, R1} = request_uri(R0),
	{[C|Rest], R1}.

http_version([$H, $T, $T, $P, $/, $1, $., $1 |R0]) ->
	{v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 |R0]) ->
	{v10, R0}.

headers([13,10 |R0]) ->
	{[], R0}; %% On a atteint la fin des headers
headers(R0) ->
	{Header, R1} = header(R0), %% On divise les headers en plusieurs string 
	{Rest, R2} = headers(R1), %% On prend le reste des headers et on réapplique la "division"
	{[Header|Rest], R2}.

header([13,10 |R0]) ->
	{[], R0}; %% On a atteint la fin des headers 
header([C |R0]) ->
	{Rest, R1} = header(R0), %% On continue d'étudier les headers un par un car le dernier header étudier n'est pas le dernier header
	{[C|Rest], R1}.

message_body(R) ->
	{R, []}.

ok(Body) ->
	"HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

get(URI) ->
	"GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".