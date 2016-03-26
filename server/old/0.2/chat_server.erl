-module(chat_server).
-author('Robert Holt').

-include("include/server_structure.hrl").
-include("include/messages.hrl").

-record(occupant, {pid, name}).

% Server API
-export([start/1, stop/0, new_connection/1, handle_msg/2]).

% Callbacks for gen_server
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

% Server API definitions
start(Port) -> gen_server:start_link({local, ?SERVER},
                                     ?MODULE,
                                     [Port],
                                     [process_flag(trap_exit, true)]).
stop() -> gen_server:call(?MODULE, stop).
handle_msg(Pid, Msg) -> gen_server:cast(?MODULE, {Pid, Msg}).
new_connection(Pid) -> gen_server:cast(?MODULE, {Pid, new_connection}).

% ========== General server definitions =================
init([Port]) ->
    InitState = chat_state:initialise(),
    chat_room:create(InitState, ?MAINHALL, <<"">>),
    spawn_link(chat_listener, start, [Port, self()]),
    {ok, InitState}.

% TODO: Put connection shutdown here perhaps, or just link processes
handle_call(stop, _From, State) -> {stop, normal, stopped, State};
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> init:stop().
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% ========= Server functionality -- Cast handling ==========
handle_cast({From, new_connection}, State) ->
    Name = chat_user:create(State, From),
    From ! chat_message:newidentity(self(), Name, <<>>),
    From ! chat_message:roomchange(self(), Name, ?MAINHALL, <<>>),
    chat_room:add_user(State, ?MAINHALL, From, Name),
    {noreply, State};

handle_cast({From, #join{roomid=RoomID}}, State) ->
    UName = chat_user:get_name(State, From),
    Former = chat_user:get_room(State, From),
    case chat_room:exists(RoomID) of
        true ->
            Msg = chat_message:roomchange(self(), UName, RoomID, Former),
            chat_room:send(State, Former, Msg),
            chat_room:send(State, RoomID, Msg),
            chat_user:change_room(State, From, RoomID);
        false ->
            From ! chat_message:roomchange(self(), UName, Former, Former)
    end,
    {noreply, State};

handle_cast({From, #delete{roomid=RoomID}}, State) ->
    case chat_room:exists(State, RoomID) and 
         chat_room:is_owned_by(State, RoomID, From) of
        true ->
            MsgFun = fun (User, _) ->
                             UName = User#occupant.name,
                             Msg = chat_message:roomchange(self(),
                                                           UName,
                                                           ?MAINHALL,
                                                           RoomID),
                             User#occupant.pid ! Msg
                     end,
            chat_room:fold_all(State, RoomID, MsgFun, not_used),
            chat_room:move_all(State, RoomID, ?MAINHALL);
        false ->
            no_action
    end,
    RmList = chat_room:list_all(State),
    From ! chat_message:roomlist(self(), RmList),
    {noreply, State};

handle_cast({From, #kick{roomid=RoomID, time=Time, identity=Ident}}, State) ->
    case chat_room:is_owned_by(State, RoomID, From) of
        true ->
            UPid = chat_user:get_pid(State, Ident),
            OldRoomID = chat_user:get_room(State, Ident),
            Msg = chat_message:roomchange(self(),Ident,?MAINHALL,RoomID),
            chat_user:send(State, UPid, Msg),
            chat_room:move_user(State, OldRoomID, RoomID, UPid),
            chat_room:ban(State, RoomID, UPid, Time);
        false ->
            no_action
    end,
    RmContents = chat_room:list_in_room(State, RoomID),
    Owner = chat_room:get_owner(State, RoomID),
    From ! chat_message:roomcontents(self(),RoomID,Owner,RmContents),
    {noreply, State};

handle_cast({From, #message{content=Content}}, State) ->
    UName = chat_user:get_name(From),
    RoomID = chat_user:get_room(State, From),
    Msg = chat_message:message(self(),Content,UName),
    chat_room:send(State, RoomID, Msg),
    {noreply, State};

handle_cast({From, #identitychange{identity=Identity}}, State) ->
    UName = chat_user:get_name(State, From),
    case chat_user:name_taken(State, Identity) of
        true ->
            From ! chat_message:newidentity(self(),UName, UName);
        false ->
            Msg = chat_message:newidentity(self(),Identity, UName),
            RoomID = chat_user:get_room(State, From),
            chat_room:send(State, RoomID, Msg)
    end,
    {noreply, State};

handle_cast({From, list}, State) ->
    RmList = chat_room:list_all(State),
    From ! chat_message:roomlist(self(), RmList);

handle_cast({From, quit}, State) ->
    UName = chat_user:get_name(State, From),
    RoomID = chat_user:get_room(State, From),
    Msg = chat_message:roomchange(UName, <<>>, RoomID),
    chat_room:send(State, RoomID, Msg),
    chat_room:delete_user(State, RoomID, From),
    chat_user:delete(State, From),
    chat_room:remove_owner(State, From);

handle_cast({From, Msg}, State) ->
    Reply = io_lib:format("~s: ~p~n", ["Unsupported message", Msg]),
    ReplyBin = unicode:characters_to_binary(Reply),
    From ! #error{message=ReplyBin},
    {noreply, State}.
