-module(chat_connection).
-export([start/2]).

-include("messages.hrl").
-include("server_structure.hrl").

start(Listen, ServerPid) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    SendPid = spawn_link(fun send/2, [Socket, ServerPid]),
    rpc:cast(?NODE, ?SERVER, join, [SendPid, ?MAINHALL]),
    listen(Socket).

send(Socket, ServerPid) ->
    receive
        {chat, ServerPid, Msg} ->
            Bin = json:to_bin(Msg),
            gen_tcp:send(Socket, Bin)
    end,
    send(Socket, ServerPid).

listen(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            Msg = json:to_tuple(Bin),
            rpc:cast(?NODE, ?SERVER, handle_msg, [self(), Msg])
    end,
    listen(Socket).
