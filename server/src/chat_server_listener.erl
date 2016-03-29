-module(chat_server_listener).

-author('Robert Holt').

-export([start/0, start/1]).

start(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {reuseaddr, true}]),
    %TODO: Set process monitoring
    spawn(fun () -> par_connect(ListenSocket) end).

par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    erlang:display("Accepted"),
    % Respawn a fresh listening process
    %TODO: Set up process monitoring
    spawn(fun () -> par_connect(Listen) end),
    erlang:display("Spawned next"),
    % Get on with the initial client handshake
    SendPid = spawn_link(chat_server_sender, start, [Socket]),
    erlang:display("Started sender"),
    % Start the client receiving process
    chat_server_receiver:start(Socket, SendPid).

start() ->
    spawn(fun () -> process_par_connect() end).

process_par_connect() ->
    Pid = process_accept(),
    spawn(fun () -> process_par_connect() end),
    SendPid = spawn_link(chat_server_sender, start, [Pid]),
    chat_server_receiver:start(Pid, SendPid).

process_accept() ->
    receive
        {new_connection, Pid} when is_pid(Pid) ->
            Pid
    end.
