-module(chat_state).
-author('Robert Holt').

-include("messages.hrl").
-include("server_structure.hrl").

-export([initialise/0,
         message/3,
         add_client/3,
         quit_client/2,
         change_client_room/3,
         reject_roomchange/2,
         room_exists/2,
         change_id/3,
         reject_idchange/2,
         client_not_new/2,
         name_in_use/2]).

% =========== EXPORTED FUNCTIONS ==============

% ----- Start Up -----
%
initialise() ->
    InitState = #state{},
    ets:insert(InitState#state.rooms, #room{name=?MAINHALL, owner=''}),
    InitState.

% ----- Message Type: message -----
%
message(State, Content, CPid) ->
    [Client] = ets:lookup(State#state.clients, CPid),
    [Room] = ets:lookup(State#state.rooms, Client#client.room),
    Name = Client#client.name,
    Msg = #serv_msg{content=Content,identity=Name},
    send(Room#room.occupants, Msg).

% ----- Message Type: join -----
%
add_client(State, RoomID, CPid) ->
    Name = new_guest(State#state.clients),
    Client = #client{pid=CPid,name=Name,room=RoomID},
    ets:insert(State#state.clients, Client),
    [Room] = ets:lookup(State#state.rooms, RoomID),
    ets:insert(Room#room.occupants, Client),
    ets:insert(State#state.names, {Name}),
    Msg = #roomchange{identity=Name,roomid=RoomID,former=''},
    send(Room#room.occupants, Msg).

quit_client(State, CPid) ->
    [Client] = ets:lookup(State#state.clients, CPid),
    [Room] = ets:lookup(State#state.rooms, Client#client.room),
    ets:delete(State#state.clients, CPid),
    ets:delete(Room#room.occupants, CPid),
    ets:delete(State#state.names, Client#client.name),
    change_owner(Client#client.owns, State#state.rooms, ''),
    Name = Client#client.name,
    PrevRoomID = Client#client.room,
    Msg = #roomchange{identity=Name,roomid='',former=PrevRoomID},
    CPid ! Msg,
    send(Room#room.occupants, Msg).

change_client_room(State, RoomID, CPid) ->
    [Client] = ets:lookup(State#state.clients, CPid),
    [PrevRoom] = ets:lookup(State#state.rooms, Client#client.room),
    [Room] = ets:lookup(State#state.rooms, RoomID),
    ets:update_element(State#state.clients, CPid, {#client.room, RoomID}),
    ets:delete(PrevRoom#room.occupants, CPid),
    ets:insert(Room#room.occupants, Client),
    Name = Client#client.name,
    PrevRoomID = PrevRoom#room.name,
    Msg = #roomchange{identity=Name,roomid=RoomID,former=PrevRoomID},
    send(PrevRoom#room.occupants, Msg),
    send(Room#room.occupants, Msg).

reject_roomchange(State, CPid) ->
    [Client] = ets:lookup(State#state.clients, CPid),
    Name = Client#client.name,
    RoomID = Client#client.room,
    Msg = #roomchange{identity=Name,roomid=RoomID,former=RoomID},
    CPid ! Msg.

client_not_new(State, CPid) ->
    ets:member(State#state.clients, CPid).

% ----- Message Type: identitychange -----
%
change_id(State, Ident, CPid) ->
    [Client] = ets:lookup(State#state.clients, CPid),
    [Room] = ets:lookup(State#state.rooms, Client#client.room),
    PrevName = Client#client.name,
    ets:update_element(State#state.clients, CPid, {#client.name, Ident}),
    ets:delete(State#state.names, PrevName),
    ets:insert(State#state.names, {Ident}),
    Msg = #newidentity{identity=Ident,former=PrevName},
    send(Room#room.occupants, Msg).

reject_idchange(State, CPid) ->
    [Client] = ets:lookup(State#state.clients, CPid),
    [Room] = ets:lookup(State#state.rooms, Client#client.room),
    Name = Client#client.name,
    Msg = #newidentity{identity=Name,former=Name},
    send(Room#room.occupants, Msg).

% ----- General Functions -----
%
room_exists(State, RoomID) ->
    ets:member(State#state.rooms, RoomID).

name_in_use(State, Name) ->
    ets:member(State#state.names, Name).

% ----------- INTERNAL FUNCTIONS -----------------

% Sends Msg to all clients in the ets table Clients
send(Clients, Msg) ->
    SendEach = fun (#client{pid=CPid}, _) ->
                       CPid ! Msg
               end,
    ets:foldl(SendEach, not_used, Clients).

change_owner(Rms, RoomTab, NewName) ->
    RmvOwner = fun (Rm) ->
                       ets:update_element(RoomTab,
                                          Rm, {#room.owner, NewName})
               end,
    lists:map(RmvOwner, Rms).

new_guest(ClientTab) ->
    Nums = ets:foldl(fun get_guest_num_list/2, [], ClientTab),
    SortedNums = lists:sort(Nums),
    NewGuestNum = lowest_not_in_list(SortedNums),
    "guest" ++ integer_to_list(NewGuestNum).

lowest_not_in_list(Nums) ->
    lowest_acc(Nums, 1).
lowest_acc([N|Ns], Acc) ->
    if
        Acc < N -> Acc;
        true    -> lowest_acc(Ns, Acc+1)
    end;
lowest_acc([], Acc) -> Acc.

get_guest_num_list({_From, Client}, NumList) ->
    Name = Client#client.name,
    RE = "guest\\d+",
    case re:run(Name, RE, [{capture, none}]) of
        nomatch ->
            NumList;
        match ->
            % Remove "guest" from "guest\\d+" and turn it into a number
            Num = list_to_integer(string:substr(Name,6)),
            [Num|NumList]
    end.
