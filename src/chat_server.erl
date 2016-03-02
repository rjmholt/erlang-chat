-module(chat_server).
-author('Robert Holt').

-behaviour(gen_server).

-define(SERVER, ?MODULE).

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
    InitState = chat_state:initialise(),
    {ok, InitState}.

% Message: join
% Behaviour: Client joins Room, and leaves room they
%   were previously in, if any. If the room is invalid,
%   Client is sent a roomchange to the room they are
%   currently in.
handle_cast({#join{roomid=RoomID}, From}, State) ->
    % Make sure the room exists
    % If it does, see if the client already exists
    case chat_state:room_exists(State, RoomID) of
        true ->
            case chat_state:client_not_new(State, From) of
                % If the client isn't connecting for the first
                % time, move them out of the old room and
                % notify its occupants
                true ->
                    case chat_state:is_banned(State, RoomID, From) of
                        true ->
                            chat_state:reject_roomchange(State, From);
                        false ->
                            chat_state:change_client_room(State, RoomID, From);
                false ->
                    % Otherwise they are connecting for the first time
                    % and have to be added to the state
                    chat_state:add_client(State, RoomID, From)
            end;
        % If the room doesn't exist, then the request is invalid,
        % so is rejected
        false ->
            chat_state:reject_roomchange(State, From)
    end,
    {noreply, State};

handle_cast({quit, From},  State) ->
    chat_state:quit_client(State, From),
    {noreply, State};

handle_cast({#message{content=Content}, From}, State) ->
    chat_state:message(State, Content, From),
    {noreply, State};

handle_cast({#identitychange{identity=Name}, From}, State) ->
    case chat_state:name_in_use(State, Name) of
        true ->
            chat_state:reject_idchange(State, From);
        false ->
            chat_state:change_id(State, Name, From)
    end,
    {noreply, State};

handle_cast({#createroom{identity=Name,roomid=RoomID}, From}, State) ->
    case chat_state:room_exists(RoomID) of
        true ->
            chat_state:reject_createroom(State, From);
        false ->
            chat_state:make_room(State, RoomID, Name, From)
    end,
    {noreply, State};

handle_cast({#delete{roomid=RoomID}, From}, State) ->
    case chat_state:room_exists(State, RoomID) of
        true ->
            case chat_state:is_owner(State, RoomID, From) of
                true ->
                    chat_state:delete_room(State, RoomID);
                false ->
                    chat_state:send_roomlist(State, From)
            end;
        false ->
            chat_state:send_roomlist(State, From)
    end,
    {noreply, State};

handle_cast({list, From}, State) ->
    chat_state:send_roomlist(State, From),
    {noreply, State};

handle_cast({#who{roomid=RoomID}, From}, State) ->
    chat_state:send_roomcontents(State, RoomID, From),
    {noreply, State};

handle_cast({#kick{roomid=RoomID,time=Time,identity=Identity},From},State) ->
    case chat_state:is_owner(State, RoomID, From) of
        true ->
            chat_state:kick_client(State, Identity, RoomID, Time, From)
    end,
    chat_state:send_roomcontents(State, RoomID, From),
    {noreply, State};

handle_cast({Msg, Pid}, State) ->
    MsgStr = io_lib:format("~p", Msg),
    Pid ! #error{content="Unsupported message" ++ MsgStr},
    {noreply, State}.

handle_call(stop, _From, State) -> {stop, normal, stopped, State};
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
