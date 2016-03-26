-module(chat_server_sender).

-author('Robert Holt').

-export([start/1, send/2]).

start(Socket) ->
    loop(Socket).

loop(Socket) ->
    receive
        {send, Msg} ->
            erlang:display("Encoding"),
            Bin = jiffy:encode(Msg),
            erlang:display("Encoding done"),
            gen_tcp:send(Socket, Bin)
    end,
    loop(Socket).

send(SenderPid, Msg) ->
    SenderPid ! {send, Msg}.
