-module(chat_listner).
-export([start/2]).

start(Port, ServerPid) ->
    loop(Port, ServerPid).

loop(Port, ServerPid) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {reuseaddr, true}]),
    spawn(chat_connection, start, [Listen, ServerPid]),
    loop(Port, ServerPid).
