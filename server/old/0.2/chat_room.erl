-module(chat_room).
-author('Robert Holt').

-include("include/server_structure.hrl").

-record(room, {name, owner, users=room_init_users(), bans=[]}).
-record(occupant, {pid, name}).

-export([initialise/1,
         send/3,
         create/3,
         delete/2,
         exists/2,
         add_user/4,
         delete_user/3,
         move_user/4,
         move_all/3,
         fold_all/4,
         already_exists/2,
         get_owner/2,
         is_owned_by/3,
         remove_owner/2,
         ban/4,
         list_in_room/2,
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

exists(State, RoomID) ->
    RoomTab = state_get_rooms(State),
    rooms_member(RoomTab, RoomID).

add_user(State, RoomID, UPid, UserName) ->
    RoomTab = state_get_rooms(State),
    Room = rooms_lookup(RoomTab, RoomID),
    room_add_user(Room#room.users, #occupant{pid=UPid,name=UserName}).

delete_user(State, RoomID, UPid) ->
    RoomTab = state_get_rooms(State),
    Room = rooms_lookup(RoomTab, RoomID),
    room_take_user(Room#room.users, UPid).

move_user(State, OldRoomID, NewRoomID, UPid) when is_pid(UPid) ->
    RoomTab = state_get_rooms(State),
    OldRoom = rooms_lookup(RoomTab, OldRoomID),
    User = room_take_user(OldRoom#room.users, UPid),
    NewRoom = rooms_lookup(RoomTab, NewRoomID),
    room_add_user(NewRoom#room.users, User).

move_all(State, OldRoomID, NewRoomID) ->
    RoomTab = state_get_rooms(State),
    OldRoom = rooms_lookup(RoomTab, OldRoomID),
    GetUsers = fun (User, Acc) -> [User|Acc] end,
    Users = room_fold_users(OldRoom#room.users, GetUsers, []),
    NewRoom = rooms_lookup(RoomTab, NewRoomID),
    AddUser = fun (User) ->
                       room_add_user(NewRoom#room.users, User)
               end,
    lists:map(AddUser, Users).

fold_all(State, RoomID, Fun, Acc) ->
    RoomTab = state_get_rooms(State),
    Room = rooms_lookup(RoomTab, RoomID),
    room_fold_users(Room#room.users, Fun, Acc).

already_exists(State, RoomID) ->
    RoomTab = state_get_rooms(State),
    rooms_member(RoomTab, RoomID).

get_owner(State, RoomID) ->
    RoomTab = state_get_rooms(State),
    Room = rooms_lookup(RoomTab, RoomID),
    Room#room.owner.

is_owned_by(State, RoomID, UPid) ->
    RoomTab = state_get_rooms(State),
    Room = rooms_lookup(RoomTab, RoomID),
    Room#room.owner =:= UPid.

remove_owner(State, UPid) ->
    RoomTab = state_get_rooms(State),
    FindOwnerF = fun (Room, Acc) ->
                    case Room#room.owner of
                        UPid ->
                            [Room#room.name | Acc];
                        _ ->
                            Acc
                    end
                 end,
    OwnedRooms = rooms_fold(RoomTab, FindOwnerF, []),
    UpdateOwnerF = fun (RoomID) ->
                    rooms_remove_owner(RoomTab, RoomID)
                   end,
    lists:map(UpdateOwnerF, OwnedRooms).

ban(State, RoomID, UPid, Time) ->
    RoomTab = state_get_rooms(State),
    Room = rooms_lookup(RoomTab, RoomID),
    Bans = [UPid|Room#room.bans],
    room_update_bans(RoomTab, RoomID, Bans),
    timer:apply_after(Time, ?MODULE, room_delete_ban,
                      [RoomTab, RoomID,UPid]).

list_in_room(State, RoomID) ->
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

% Room storage interface functions (mask ETS)

rooms_init() ->
    ets:new(rooms, [{keypos, #room.name}]).

rooms_insert(RoomTab, Room) ->
    ets:insert(RoomTab, Room).

rooms_delete(RoomTab, RoomID) ->
    ets:delete(RoomTab, RoomID).

rooms_member(RoomTab, RoomID) ->
    ets:member(RoomTab, RoomID).

rooms_lookup(RoomTab, RoomID) ->
    case ets:lookup(RoomTab, RoomID) of
        [Room] ->
            Room;
        [] ->
            none
    end.

rooms_remove_owner(RoomTab, RoomID) ->
    ets:update_element(RoomTab, RoomID, {#room.owner, <<>>}).

rooms_fold(RoomTab, Fun, Acc) ->
    ets:foldl(Fun, Acc, RoomTab).

room_init_users() ->
    ets:new(occupants, [{keypos, #occupant.pid}]).

room_update_bans(RoomTab, RoomID, Bans) ->
    ets:update_element(RoomTab, RoomID, {#room.bans, Bans}).

room_add_user(Occupants, User) ->
    ets:insert(Occupants, User).

room_take_user(Occupants, UPid) ->
    ets:take(Occupants, UPid).

room_fold_users(Occupants, Fun, Acc) ->
    ets:foldl(Fun, Acc, Occupants).

% State interface functions

state_get_rooms(State) ->
    chat_state:get_table(State, rooms).

state_init_rooms(State, RoomTab) ->
    chat_state:add_table(State, RoomTab, rooms).

% Auxiliary functions

room_delete_ban(RoomTab, RoomID, UPid) ->
    Room = rooms_lookup(RoomTab, RoomID),
    Bans = lists:delete(UPid, Room#room.bans),
    room_update_bans(RoomTab, RoomID, Bans).
