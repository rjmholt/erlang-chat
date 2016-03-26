-module(chat_server_nameserver).

-author('Robert Holt').

-behaviour(gen_server).

-include("include/server_structure.hrl").

-define(GUESTNAME, "guest").

% Name record structure
-record(name, {ident, pid}).
-record(room, {roomid, pid}).

% Server RPC API
-export([start/0, stop/0, new_connection/1,
         change_name/2, delete_name/1,
         get_upid/1,
         add_room/2, has_room/1, delete_room/1,
         get_roompid/1]).

% gen_server API
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

% Nameserver RPC definitions
start() -> gen_server:start_link({local, ?NAMESERVER}, ?MODULE, [], []).
stop() -> gen_server:call(?NAMESERVER, stop).
new_connection(Pid) -> gen_server:call(?NAMESERVER, {new_connection, Pid}).
change_name(Name, UPid) ->
    gen_server:call(?NAMESERVER, {change_name, Name, UPid}).
delete_name(Name) -> gen_server:call(?NAMESERVER, {del_name, Name}).
get_upid(Name) -> gen_server:call(?NAMESERVER, {get_upid, Name}).
add_room(RoomID, RPid) -> gen_server:call(?NAMESERVER, {add_room, RoomID, RPid}).
has_room(RoomID) -> gen_server:call(?NAMESERVER, {has_room, RoomID}).
delete_room(RoomID) -> gen_server:call(?NAMESERVER, {del_room, RoomID}).
get_roompid(RoomID) -> gen_server:call(?NAMESERVER, {get_roompid, RoomID}).

% gen_server handling definitions
init([]) ->
    Names = ets:new(names, [{keypos, #name.ident}]),
    Rooms = ets:new(rooms, [{keypos, #room.roomid}]),
    {ok, {Names, Rooms}}.

terminate(_Reason, _State) -> init:stop().

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

handle_call(stop, _From, State) -> {stop, normal, stopped, State};

handle_call({new_connection, Pid}, _From, {Names, Rooms}) ->
    Name = new_guest_name(Names),
    ets:insert(Names, #name{ident=Name, pid=Pid}),
    {reply, {ok, Name}, {Names, Rooms}};

handle_call({add_name, ID, UPid}, _From, {Names, Rooms}) ->
    Reply = case ets:member(Names, ID) of
        true ->
            already_exists;
        false ->
            ets:insert(Names, #name{ident=ID, pid=UPid}),
            ok
    end,
    {reply, Reply, {Names, Rooms}};

handle_call({del_name, ID}, _From, {Names, Rooms}) ->
    Reply = case ets:member(Names, ID) of
        true ->
            ets:delete(Names, ID),
            ok;
        false ->
            no_such_name
    end,
    {reply, Reply, {Names, Rooms}};

handle_call({get_upid, ID}, _From, {Names, Rooms}) ->
    Reply = case ets:lookup(Names, ID) of
                [Name] -> {ok, Name#name.pid};
                [] -> no_such_name
            end,
    {reply, Reply, {Names, Rooms}};

handle_call({add_room, RmID, RPid}, _From, {Names, Rooms}) ->
    Reply = case ets:member(Rooms, RmID) of
                true ->
                    already_exists;
                false ->
                    ets:insert(Rooms, #room{roomid=RmID, pid=RPid}),
                    ok
            end,
    {reply, Reply, {Names, Rooms}};

handle_call({has_room, RmID}, _From, {Names, Rooms}) ->
    Reply = ets:member(Rooms, RmID),
    {reply, Reply, {Names, Rooms}};

handle_call({del_room, RmID}, _From, {Names, Rooms}) ->
    Reply = case ets:member(Rooms, RmID) of
                true ->
                    ets:delete(Rooms, RmID),
                    ok;
                false ->
                    no_such_room
            end,
    {reply, Reply, {Names, Rooms}};

handle_call({get_roompid, RmID}, _From, {Names, Rooms}) ->
    Reply = case ets:lookup(Rooms, RmID) of
                [Room] -> {ok, Room#room.pid};
                [] -> no_such_room
            end,
    {reply, Reply, {Names, Rooms}}.

% Auxilliary functions 
new_guest_name(Names) ->
    Nums = ets:foldl(fun get_guest_num_list/2, [], Names),
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

get_guest_num_list(#name{ident=ID}, NumList) ->
    RE = <<?GUESTNAME/utf8,"\\d+"/utf8>>,
    NumIdx = string:len(?GUESTNAME)+1,
    case re:run(ID, RE, [{capture, none}]) of
        nomatch ->
            NumList;
        match ->
            % Remove "guest" from "guest\\d+" and turn it into a number
            Num = list_to_integer(string:substr(ID, NumIdx)),
            [Num|NumList]
    end.
