-module(min).

-export([echo/1]).

echo(Msg) ->
    io:format("~p~n", [Msg]).
