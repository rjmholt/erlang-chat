-module(chat_server_connection).

-author('Robert Holt').

-export([start/1]).

-include("include/server_structure.hrl").

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
    {Name, RmPid} = register_connection(SendPid),
    erlang:display("Registered connection"),
    send_handshake(SendPid, Name),
    erlang:display("Handshaken"),
    % Start the loops
    socket_listen(Socket, Name, RmPid).

register_connection(SendPid) ->
    {ok, Name} = chat_server_nameserver:new_connection(SendPid),
    erlang:display("Registered name"),
    {ok, RmPid} = chat_server_room:add_occupant(mainhall, SendPid),
    erlang:display("Registered room"),
    {Name, RmPid}.

socket_listen(Socket, Name, RmPid) ->
    {NewName, NewRmPid} = receive
        {tcp, Socket, Bin} ->
            Msg = jiffy:decode(Bin, [return_maps]),
            process_msg(Name, RmPid, Msg)
    end,
    socket_listen(Socket, NewName, NewRmPid).

process_msg(Name, RmPid,
            #{<<"type">> := <<"message">>,
              <<"content">> := Content}) ->
    Msg = #{type => message, identity => Name, content => Content},
    chat_server_room:send(RmPid, Msg).

send_handshake(SendPid, Name) ->
    erlang:display("Sending"),
    IdMsg = #{type => newidentity, identity => Name, former => <<>>},
    RmMsg = #{type => roomchange, identity => Name,
              roomid => ?MAINHALL, former => <<>>},
    chat_server_sender:send(SendPid, IdMsg),
    chat_server_sender:send(SendPid, RmMsg).
