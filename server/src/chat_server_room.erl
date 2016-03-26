-module(chat_server_room).

-author('Robert Holt').

-behaviour(gen_server).

% Room server state record definition
-record(room, {name, owner, occupants=[], bans=[]}).

% Room Server API
-export([create/2, create_registered/3, delete/1, add_occupant/2,
         add_occupant_list/2,
         remove_occupant/2, send/2, get_contents_list/1,
         get_room_summary/1, add_ban/2, delete_ban/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).


% Room API definitions
create(RoomID, Owner) ->
    Pid = gen_server:start_link(?MODULE, [RoomID, Owner], []),
    case rpc:call(node(), chat_server_nameserver, add_room, [RoomID, Pid]) of
        ok -> {ok, Pid};
        already_exists ->
            gen_server:call(Pid, stop),
            already_exists
    end.

create_registered(PName, RoomID, Owner) ->
    {ok, Pid} = gen_server:start_link({local, PName}, ?MODULE, [RoomID, Owner], []),
    case chat_server_nameserver:has_room(RoomID) of
        true ->
            already_exists;
        false ->
            rpc:call(node(), chat_server_nameserver, add_room, [RoomID, Pid]),
            {ok, Pid}
    end.

delete(RmPid) -> gen_server:call(RmPid, stop).

add_occupant(RmPid, UPid) -> gen_server:call(RmPid, {join, UPid}).

add_occupant_list(RmPid, UPids) -> gen_server:call(RmPid, {join, UPids}).

remove_occupant(RmPid, UPid) -> gen_server:call(RmPid, {remove, UPid}).

send(RmPid, Msg) -> gen_server:cast(RmPid, {send_all, Msg}).

get_contents_list(RmPid) -> gen_server:call(RmPid, contents_list).

get_room_summary(RmPid) -> gen_server:call(RmPid, room_summary).

add_ban(RmPid, UPid) -> gen_server:call(RmPid, {ban, UPid}).

delete_ban(RmPid, UPid) -> gen_server:cast(RmPid, {del_ban, UPid}).

% gen_server callback handling
init([Name, Owner]) ->
    {ok, #room{name=Name, owner=Owner}}.

terminate(_Reason, _State) -> init:stop().

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_info(_Info, State) -> {noreply, State}.

handle_cast({send_all, Msg}, State) ->
    SendEach = fun (UPid) -> UPid ! Msg end,
    lists:map(SendEach, State#room.occupants),
    {noreply, State};

handle_cast({del_ban, UPid}, State) ->
    NewBans = lists:delete(UPid, State#room.bans),
    {noreply, State#room{bans = NewBans}}.

handle_call(stop, _From, State) ->
    %TODO: Move all occupants to mainhall nicely
    {stop, normal, stopped, State};

handle_call({join, UPid}, _From, State) when is_pid(UPid) ->
    RmPid = chat_server_nameserver:get_roompid(State#room.name),
    {reply, {ok, RmPid},
             State#room{occupants = [UPid|State#room.occupants]}};

handle_call({join, UPids}, _From, State) when is_list(UPids) ->
    %TODO message everyone about arrival
    {reply, {ok, State#room.name},
     State#room{occupants = lists:append(UPids, State#room.occupants)}};

handle_call(leave, {UPid, _Tag}, State) ->
    Occupants = State#room.occupants,
    {reply, ok, State#room{occupants = lists:delete(UPid, Occupants)}};

handle_call(contents_list, _From, State) ->
    %TODO: Collect names of users
    OccupantNames = [],
    Resp = {roomcontents, State#room.name, State#room.owner, OccupantNames},
    {reply, Resp, State};

handle_call(room_summary, _From, State) ->
    NumOccs = length(State#room.occupants),
    {reply, {roomsummary, State#room.name, NumOccs}, State};

handle_call({ban, UPid, Time}, _From, State) ->
    timer:apply_after(Time, ?MODULE, delete_ban, [UPid]),
    {reply, ok, State#room{bans = [UPid|State#room.bans]}}.
