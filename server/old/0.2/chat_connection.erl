-module(chat_connection).
-export([start/2]).

-include("include/messages.hrl").
-include("include/server_structure.hrl").

start(Socket, ServerPid) ->
    SendPid = spawn_link(fun send/2, [Socket, ServerPid]),
    rpc:cast(?NODE, ?SERVER, new_connection, [SendPid]),
    listen(Socket).

send(Socket, ServerPid) ->
    receive
        {chat, ServerPid, Msg} ->
            Bin = jiffy:encode(Msg),
            gen_tcp:send(Socket, <<Bin/binary, "\n"/utf8>>)
    end,
    send(Socket, ServerPid).

listen(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            Json = jiffy:decode(Bin, [return_maps]),
            Msg = chat_message:convert(Json),
            rpc:cast(?NODE, ?SERVER, handle_msg, [self(), Msg])
    end,
    listen(Socket).
