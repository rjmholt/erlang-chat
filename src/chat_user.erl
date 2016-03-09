-module(chat_user).
-author('Robert Holt').

-include("include/server_structure.hrl").

-record(user, {pid, name, room=?MAINHALL, owns=[]}).
-record(name_upid, {name, pid}).

-define(GUESTNAME, "guest").

-export([initialise/1,
         send/3,
         create/2,
         delete/2,
         rename/3,
         change_room/3,
         already_exists/2,
         get_name/2]).

% initialise/1
%   - State: server state containing refs to mutable tables
%
% Initialise the server state with empty
% tables for the users and the username reverse lookup
initialise(State) ->
    UserTab = user_init_table(),
    NameTab = name_init_table(),
    NextState = state_init_users(State, UserTab),
    state_init_names(NextState, NameTab).

% send/3
%   - State: server state
%   - User: a username, pid, or list thereof
%   - Msg: message tuple to send
%
% Send a message to a user

% Send message to user pid
send(_State, UPid, Msg) when is_pid(UPid) ->
    UPid ! Msg;

% Send message to user via username
send(State, UserName, Msg) when is_binary(UserName) ->
    NameTab = state_get_names(State),
    User = name_lookup(NameTab, UserName),
    send(State, User#user.pid, Msg);

% Send message to list of users
send(State, Users, Msg) when is_list(Users) ->
    SendEach = fun (User) ->
                       send(State, User, Msg)
               end,
    lists:map(SendEach, Users).


create(State, UPid) ->
    UserTab = state_get_users(State),
    Name = new_guest_name(UserTab),
    User = #user{pid=UPid, name=Name},
    user_insert(UserTab, User),
    NameTab = state_get_names(State),
    name_insert(NameTab, #name_upid{name=Name,pid=UPid}),
    Name.

delete(State, UPid) when is_pid(UPid) ->
    UserTab = state_get_users(State),
    User = user_take(UserTab, UPid),
    room_delete_user(State, User#user.room, UPid),
    NameTab = state_get_names(State),
    name_delete(NameTab, User#user.name).
    
rename(State, UPid, NewName) ->
    UserTab = state_get_users(State),
    user_update_name(UserTab, UPid, NewName).

change_room(State, UPid, NewRoom) ->
    UserTab = state_get_users(State),
    User = user_lookup(UserTab, UPid),
    user_update_room(UserTab, UPid, NewRoom),
    room_delete_user(State, User#user.room, UPid),
    room_add_user(State, NewRoom, User#user{room=NewRoom}).

already_exists(State, UserName) ->
    NameTab = state_get_names(State),
    name_member(NameTab, UserName).

get_name(State, UPid) ->
    UserTab = state_get_users(State),
    User = user_lookup(UserTab, UPid),
    User#user.name.

%% USER STORAGE INTERFACE
%
user_init_table() ->
    ets:new(users, [{keypos, #user.pid}]).

user_insert(UserTab, User) ->
    ets:insert(UserTab, User).

user_take(UserTab, UPid) ->
    ets:take(UserTab, UPid).

user_lookup(UserTab, UPid) ->
    case ets:lookup(UserTab, UPid) of
        [User] ->
            User;
        [] ->
            none
    end.

user_update_name(UserTab, UPid, Name) ->
    ets:update_element(UserTab, UPid, {#user.name, Name}).

user_update_room(UserTab, UPid, Room) ->
    ets:update_element(UserTab, UPid, {#user.room, Room}).

%% NAME LOOKUP INTERFACE
%
name_init_table() ->
    ets:new(names, [{keypos, #name_upid.name}]).

name_insert(NameTab, NameEntry) ->
    [Name] = ets:insert(NameTab, NameEntry),
    Name.

name_delete(NameTab, Name) ->
    ets:delete(NameTab, Name).

name_lookup(NameTab, Name) ->
    case ets:lookup(NameTab, Name) of
        [Name]  -> Name;
        []      -> none
    end.

name_member(NameTab, Name) ->
    ets:member(NameTab, Name).

name_fold(NameTab, Fun, Acc) ->
    ets:foldl(Fun, Acc, NameTab).

%% STATE INTERFACE
%
state_init_users(State, UserTab) ->
    chat_state:add_table(State, UserTab, users).

state_init_names(State, NameTab) ->
    chat_state:add_table(State, NameTab, names).

state_get_users(State) ->
    chat_state:get_table(State, users).

state_get_names(State) ->
    chat_state:get_table(State, names).

%% ROOM INTERFACE
%
room_add_user(State, NewRoom, User) ->
    chat_room:add_user(State, NewRoom, User).

room_delete_user(State, Room, UPid) ->
    chat_room:delete_user(State, Room, UPid).

%% FUNCTIONS FOR GUEST NAME GENERATION
%
new_guest_name(NameTab) ->
    Nums = name_fold(fun get_guest_num_list/2, [], NameTab),
    SortedNums = lists:sort(Nums),
    NewGuestNum = lowest_not_in_list(SortedNums),
    NumBin = list_to_binary(integer_to_list(NewGuestNum)),
    <<?GUESTNAME/utf8, NumBin/binary>>.

lowest_not_in_list(Nums) -> lowest_acc(Nums, 1).
lowest_acc([N|Ns], Acc) ->
    if
        Acc < N -> Acc;
        true    -> lowest_acc(Ns, Acc+1)
    end;
lowest_acc([], Acc) -> Acc.

get_guest_num_list({Name, _UPid}, NumList) ->
    RE = <<?GUESTNAME/utf8,"\\d+"/utf8>>,
    NumIdx = string:len(?GUESTNAME)+1,
    case re:run(Name, RE, [{capture, none}]) of
        nomatch ->
            NumList;
        match ->
            % Remove "guest" from "guest\\d+" and turn it into a number
            Num = list_to_integer(string:substr(Name, NumIdx)),
            [Num|NumList]
    end.
