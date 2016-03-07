-module(chat_room).
-author('Robert Holt').

-include("include/server_structure.hrl").

-record(room, {name, owner, users=room_init_users(), bans=[]}).
-record(occupant, {pid, name}).

-export([initialise/1,
         send/3,
         create/3,
         delete/2,
         already_exists/2,
         is_owned_by/3,
         ban/4,
         get_contents/2,
         list_all/1]).

initialise(State) ->
    RoomTab = rooms_init(),
    state_init_rooms(State, RoomTab).

send(State, RoomID, Msg) ->
    SendEach = fun (User, _) ->
                       User#occupant.pid ! Msg
               end,
    Room = rooms_lookup(State, RoomID),
    room_fold_users(Room#room.users, SendEach, not_used).

create(State, RoomID, Owner) ->
    Room = #room{name=RoomID,owner=Owner},
    RoomTab = state_get_rooms(State),
    rooms_insert(RoomTab, Room).

delete(State, RoomID) ->
    RoomTab = state_get_rooms(State),
    rooms_delete(RoomTab, RoomID).

already_exists(State, RoomID) ->
    RoomTab = state_get_rooms(State),
    rooms_member(RoomTab, RoomID).

is_owned_by(State, RoomID, UPid) ->
    RoomTab = state_get_rooms(State),
    Room = rooms_lookup(RoomTab, RoomID),
    Room#room.owner =:= UPid.

ban(State, RoomID, UPid, Time) ->
    RoomTab = state_get_rooms(State),
    Room = rooms_lookup(RoomTab, RoomID),
    Bans = [UPid|Room#room.bans],
    room_update_bans(RoomTab, RoomID, Bans),
    timer:apply_after(Time, ?MODULE, room_delete_ban,
                      [RoomTab, RoomID,UPid]).

get_contents(State, RoomID) ->
    RoomTab = state_get_rooms(State),
    GetNames = fun (User, Acc) ->
                       Name = User#occupant.name,
                       [Name|Acc]
               end,
    Room = rooms_lookup(RoomTab, RoomID),
    Users = Room#room.users,
    UserList = room_fold_users(Users, GetNames, []),
    Owner = Room#room.owner,
    {Owner, UserList}.

list_all(State) ->
    RoomTab = state_get_rooms(State),
    GetRoomIDs = fun (Room, Acc) ->
                         RoomID = Room#room.name,
                         [RoomID|Acc]
                 end,
    rooms_fold(RoomTab, GetRoomIDs, []).

rooms_init() ->
    ets:new(rooms, [{keypos, #room.name}]).

rooms_insert(RoomTab, Room) ->
    ets:insert(RoomTab, Room).

rooms_delete(RoomTab, RoomID) ->
    ets:delete(RoomTab, RoomID).

rooms_member(RoomTab, RoomID) ->
    ets:member(RoomTab, RoomID).

rooms_lookup(RoomTab, RoomID) ->
    case ets:member(RoomTab, RoomID) of
        [Room] ->
            Room;
        [] ->
            none
    end.

rooms_fold(RoomTab, Fun, Acc) ->
    ets:foldl(Fun, Acc, RoomTab).

room_init_users() ->
    ets:new(occupants, [{keypos, #occupant.pid}]).

room_update_bans(RoomTab, RoomID, Bans) ->
    ets:update_element(RoomTab, RoomID, {#room.bans, Bans}).

room_fold_users(Occupants, Fun, Acc) ->
    ets:foldl(Fun, Acc, Occupants).

state_get_rooms(State) ->
    chat_state:get_table(State, rooms).

state_init_rooms(State, RoomTab) ->
    chat_state:add_table(State, RoomTab, rooms).

room_delete_ban(RoomTab, RoomID, UPid) ->
    Room = rooms_lookup(RoomTab, RoomID),
    Bans = lists:delete(UPid, Room#room.bans),
    room_update_bans(RoomTab, RoomID, Bans).
