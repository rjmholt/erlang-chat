-module(min_chat_server).
-author('Robert Holt').

-behaviour(gen_server).

-define(SERVER, min_chat_server).
-define(MAINHALL, 'MainHall').

-include("server_structure.hrl").
-include("messages.hrl").

% Server API
-export([start/0, stop/0, handle_msg/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

% Server API definitions
start() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).
handle_msg(Msg, Pid) -> gen_server:cast(?MODULE, {Msg, Pid}).

% gen_server implementations

init([]) -> 
    InitState = #state{},
    ets:insert(InitState#state.rooms, #room{name=?MAINHALL, owner=''}),
    {ok, InitState}.

% Message: join
% Behaviour: Client joins Room, and leaves room they
%   were previously in, if any. If the room is invalid,
%   Client is sent a roomchange to the room they are
%   currently in.
handle_cast({#join{roomid=Room}, From}, State) ->
    % Get client and room tables
    ClientTab = State#state.clients,
    RoomTab = State#state.rooms,
    % Make sure the room exists
    RoomExists = ets:member(RoomTab, Room),
    % If it does, see if the client already exists
    if
        RoomExists ->
            RTab = ets:lookup(RoomTab, Room),
            ClientExists = ets:member(ClientTab, From),
            Message = if
                % If the client exists, move them and notify
                % all in old and new rooms
                ClientExists ->
                              Client = ets:lookup(ClientTab, From),
                    PrevRoom = Client#client.room,
                    Msg = #roomchange{identity=Client,
                                      roomid=Room, former=PrevRoom},
                    ets:update_element(ClientTab, From, {#client.room, Room}),
                    PRTab = ets:lookup(RoomTab, PrevRoom),
                    serv_funcs:send(PRTab#room.occupants, Msg),
                    Msg;
                % Otherwise, the client is joining the server and
                % need a new name
                true ->
                    Name = serv_funcs:new_guest(ClientTab),
                    #roomchange{identity=Name, roomid=Room, former=''}
            end,
            serv_funcs:send(RTab#room.occupants, Message);
        % Otherwise, the room doesn't exist and the change is invalid
        true ->
            Client = ets:lookup(ClientTab, From),
            CName = Client#client.name,
            CRoom = Client#client.room,
            From ! #roomchange{identity=CName, roomid=CRoom, former=CRoom}
    end,
    {noreply, State};

handle_cast({quit, From},  State) ->
    Client = ets:lookup(State#state.clients, From),
    Name = Client#client.name,
    Room = Client#client.room,
    Msg = {roomchange, Name, '', Room},
    RmTab = ets:lookup(State#state.rooms, Room),
    serv_funcs:send(RmTab#room.occupants, Msg),
    ets:delete(State#state.clients, From),
    serv_funcs:remove_owner(Client#client.owns, State#state.rooms),
    {noreply, State};

handle_cast({#message{contents=Content}, From}, State) ->
    Client = ets:lookup(State#state.clients, From),
    Name = Client#client.name,
    Msg = {message, Content, Name},
    Room = ets:lookup(State#state.rooms, Client#client.room),
    serv_funcs:send(Room#room.occupants, Msg),
    {noreply, State};

handle_cast(_Msg, State) -> {noreply, State}.

handle_call(stop, _From, State) -> {stop, normal, stopped, State};
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
