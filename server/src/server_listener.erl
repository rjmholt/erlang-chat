-module(server_listener).

-author('Robert Holt').

-export([start/1]).

start(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {reuseaddr, true}]),
    spawn(fun () -> par_connect(ListenSocket) end).

par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun () -> par_connect(Listen) end),
    server_user:start(Socket).
