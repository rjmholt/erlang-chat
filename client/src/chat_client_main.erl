-module(chat_client_main).

-author('Robert Holt').

-behaviour(gen_server).

-include("include/client_structure.hrl").

-export([start/2, deliver/1]).

-export([init/1, handle_call/3, handle_cast/2,
        handle_info/2, terminate/2, code_change/3]).

% Client API
start(HostName, Port) ->
    gen_server:start_link({local, ?CLIENT}, ?MODULE, [HostName, Port], []).

deliver(Msg) -> gen_server:cast(?CLIENT, {chat, Msg}).

% gen_server definitions
init([HostName, Port]) ->
    Connection = chat_client_in:start(HostName, Port),
    {Name, Room} = await_welcome(Connection),
    {ok, Name, Room}.

terminate(_Reason, _State) -> init:stop().

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(_Info, State) -> {noreply, State}.

handle_call(stop, _From, State) -> {stop, normal, stopped, State}.

handle_cast({chat, #{<<"type">> := <<"message">>,
                     <<"identity">> := Identity,
                     <<"content">> := Content}}, {Name, Room}) ->
    io:format("~s: ~s", [Identity, Content]),
    {noreply, {Name, Room}}.

await_welcome(Connection) ->
    Name = receive 
               {Connection,
                #{<<"type">> := <<"newidentity">>,
                  <<"identity">> := ID,
                  <<"former">> := <<>>}} -> ID
           after
               2000 ->
                   erlang:display("Ident handshake timeout"),
                   init:stop()
           end,
    Room = receive
               {Connection, #{<<"type">> := <<"roomchange">>,
                              <<"roomid">> := Rm,
                              <<"former">> := <<>>}} -> Rm
           after
               2000 ->
                   erlang:display("Room handshake timeout"),
                   init:stop()
           end,
    {Name, Room}.

operate(PrevName, PrevRoom, Msg) ->
    case Msg of
        #{<<"type">> := <<"message">>,
         <<"content">> := Content,
         <<"identity">> := Identity} ->
            io:format("~s: ~p~n", [Identity, Content]),
            {PrevName, PrevRoom};
        #{<<"type">> := <<"newidentity">>,
          <<"identity">> := Identity,
          <<"former">> := Former} ->
            if
                Identity =:= Former ->
                    io:format("Invalid identitychange command", []),
                    {PrevName, PrevRoom};
                Former =:= PrevName ->
                    io:format("~s is now ~s~n", [Former, Identity]),
                    {Identity, PrevRoom};
                true ->
                    io:format("~s is now ~s~n", [Former, Identity]),
                    {PrevName, PrevRoom}
            end;
        #{<<"type">> := <<"roomchange">>,
          <<"identity">> := Identity,
          <<"roomid">> := RoomID,
          <<"former">> := Former} ->
            if
                RoomID =:= Former ->
                    io:format("Invalid roomchange command", []),
                    {PrevName, PrevRoom};
                Identity =:= PrevName ->
                    io:format("~s moved from ~s to ~s~n",
                              [Identity, Former, RoomID]),
                    {PrevName, RoomID};
                true ->
                    io:format("~s moved from ~s to ~s~n",
                              [Identity, Former, RoomID]),
                    {PrevName, PrevRoom}
            end;
        #{<<"type">> := <<"roomlist">>,
          <<"rooms">> := Rooms} ->
            print_roomlist(Rooms),
            {PrevName, PrevRoom};
        #{<<"type">> := <<"roomcontents">>,
          <<"roomid">> := RoomID,
          <<"owner">> := Owner,
          <<"identities">> := Identities} ->
            print_roomcontents(RoomID, Owner, Identities),
            {PrevName, PrevRoom};
        #{<<"type">> := <<"error">>,
          <<"message">> := Msg} ->
            io:format("Server error: ~p~n", [Msg]),
            {PrevName, PrevRoom};
        Msg ->
            io:format("Unsupported message: ~p~n", [Msg]),
            {PrevName, PrevRoom}
    end.

print_roomlist([{RmName, NumOcc}]) -> io:format("~s: ~s~n", [RmName, NumOcc]);
print_roomlist([{RmName, NumOcc}|RmList]) ->
    io:format("~s: ~s, ", [RmName, NumOcc]),
    print_roomlist(RmList).

print_roomcontents(RoomID, Owner, Occs) ->
    io:format("~s contains", [RoomID]),
    print_room_occupants(Owner, Occs).

print_room_occupants(_, []) -> io:format("~n");
print_room_occupants(Owner, [Occ]) ->
    case Occ of
        Owner -> io:format(" ~s*", [Owner]);
        _     -> io:format(" ~s", [Occ])
    end;
print_room_occupants(Owner, [Occ|OccList]) ->
    case Occ of
        Owner -> io:format(" ~s*", [Owner]);
        _     -> io:format(" ~s", [Occ])
    end,
    print_room_occupants(Owner, OccList).