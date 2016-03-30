-module(chat_server_sender).

-author('Robert Holt').

-export([start/1, send/2]).

start(Pid) when is_pid(Pid) ->
    process_loop(Pid);

start(Socket) ->
    socket_loop(Socket).

socket_loop(Socket) ->
    receive
        {send, Msg} ->
            erlang:display("Encoding"),
            Bin = jiffy:encode(Msg),
            erlang:display("Encoding done"),
            gen_tcp:send(Socket, <<Bin/binary, "\n"/utf8>>)
    end,
    socket_loop(Socket).

process_loop(Pid) ->
    receive {send, Msg} ->
                erlang:display(Msg),
                Pid ! Msg
    end,
    process_loop(Pid).

send(SenderPid, Msg) ->
    SenderPid ! {send, Msg}.
