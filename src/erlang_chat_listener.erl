-module(erlang_chat_listener).

-author('Robert Holt').

-export([start/1]).

start(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {reuseaddr, true}]),
    spawn(fun () -> par_connect(ListenSocket) end).

par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun () -> par_connect(Listen) end),
    SendPid = spawn_link(erlang_chat_sender, start, [Socket]),
    erlang_chat_receiver:start(Socket, SendPid).
