-module(chat_listener).
-export([start/2]).

start(Port, ServerPid) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {reuseaddr, true}]),
    loop(Listen, ServerPid).

loop(Listen, ServerPid) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(chat_connection, start, [Socket, ServerPid]),
    loop(Listen, ServerPid).
