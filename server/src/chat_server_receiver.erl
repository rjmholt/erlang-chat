-module(chat_server_receiver).

-author('Robert Holt').

-include("include/server_structure.hrl").

-export([start/2]).

start(RecvPid, SendPid) when is_pid(RecvPid) ->
    {Name, RmPid} = register_connection(SendPid),
    send_handshake(SendPid, Name),
    process_receive(RecvPid, SendPid, Name, RmPid);

start(Socket, SendPid) ->
    {Name, RmPid} = register_connection(SendPid),
    send_handshake(SendPid, Name),
    socket_receive(Socket, SendPid, Name, RmPid).

socket_receive(Socket, SendPid, Name, RmPid) ->
    {NewName, NewRmPid} =
    receive
        {tcp, Socket, Bin} ->
            Msg = jiffy:decode(Bin, [return_maps]),
            process_msg(SendPid, Name, RmPid, Msg)
    end,
    socket_receive(Socket, SendPid, NewName, NewRmPid).

process_receive(Pid, SendPid, Name, RmPid) ->
    {NewName, NewRmPid} =
    receive
        {Pid, Msg} ->
            process_msg(SendPid, Name, RmPid, Msg)
    end,
    process_receive(Pid, SendPid, NewName, NewRmPid).

register_connection(SendPid) ->
    {ok, Name} = chat_server_nameserver:new_connection(SendPid),
    {ok, RmPid} = chat_server_room:add_occupant(mainhall, SendPid),
    {Name, RmPid}.

send_handshake(SendPid, Name) ->
    IdMsg = #{<<"type">> => <<"newidentity">>,
              <<"identity">> => Name,
              <<"former">> => <<>>},
    chat_server_sender:send(SendPid, IdMsg),
    RmMsg = #{<<"type">> => <<"roomchange">>,
              <<"identity">> => Name,
              <<"roomid">> => ?MAINHALL,
              <<"former">> => <<>>},
    chat_server_sender:send(SendPid, RmMsg).

process_msg(_, Name, RmPid,
            #{<<"type">> := <<"message">>,
              <<"content">> := Content}) ->
    Resp = #{<<"type">> => <<"message">>,
             <<"content">> => Content, <<"identity">> => Name},
    chat_server_room:send(RmPid, Resp).
