-module(chat_client_in).

-author('Robert Holt').

-include("include/client_structure.hrl").

-export([start/2, start/3]).

start(Pid, HostName, Port) ->
    {ok, Socket} = gen_tcp:connect(HostName, Port, [binary]),
    handshake(Pid, Socket),
    spawn_link(chat_client_out, start, [Socket]),
    loop(Pid, Socket).

loop(Pid, Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("msg recevied: ~p~n", [Bin]),
            Msg = jiffy:decode(Bin, [return_maps]),
            chat_client_main:deliver(Pid, Msg)
    end,
    loop(Pid, Socket).

handshake(Pid, Socket) ->
    receive
        {tcp, Socket, Bin1} ->
            Msg1 = jiffy:decode(Bin1, [return_maps]),
            Pid ! {init_name, Msg1}
    end,
    receive
        {tcp, Socket, Bin2} ->
            Msg2 = jiffy:decode(Bin2, [return_maps]),
            Pid ! {init_room, Msg2}
    end.

start(Pid, ServPid) ->
    ServPid ! {new_connection, self()},
    process_handshake(Pid, ServPid),
    spawn_link(chat_client_procout, start, [ServPid]),
    process_loop(Pid, ServPid).

process_loop(Pid, ServPid) ->
    receive
        Msg ->
            erlang:display(Msg),
            chat_client_main:deliver(Pid, Msg)
    end,
    process_loop(Pid, ServPid).

process_handshake(Pid, _ServPid) ->
    receive
        Msg1 ->
            erlang:display(Msg1),
            Pid ! {init_name, Msg1}
    end,
    receive
        Msg2 ->
            erlang:display(Msg2),
            Pid ! {init_room, Msg2}
    end.
