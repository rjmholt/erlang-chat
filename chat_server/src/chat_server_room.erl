%%%-------------------------------------------------------------------
%%% @author rob
%%% @copyright (C) 2016, rob
%%% @doc
%%%
%%% @end
%%% Created : 2016-06-03 22:13:26.265562
%%%-------------------------------------------------------------------
-module(chat_server_room).

-behaviour(gen_server).

%% API
-export([new/2,
         chat_message/3,
         new_join/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {roomid, users = #{}, owner = <<>>}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new room service
%%
%% @spec new() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
new(ServerName, Owner) ->
    gen_server:start_link(?MODULE, [ServerName, Owner], []).

%%--------------------------------------------------------------------
%% @doc
%% Sends a message to all users in the room
%%
%% @spec chat_message(RoomPid, SenderName, Message) -> noreply
%% @end
%%--------------------------------------------------------------------
chat_message(RoomPid, SenderName, Message) ->
    gen_server:cast(RoomPid, {chat_message, SenderName, Message}).

%%--------------------------------------------------------------------
%% @doc
%% Join a new user to a room for the first time
%%
%% @spec new_join(RoomPid, SenderPid, SenderName) -> noreply
%% @end
%%--------------------------------------------------------------------
new_join(RoomPid, SenderPid, SenderName) ->
  gen_server:cast(RoomPid, {new_join, SenderName, SenderPid}).


%%--------------------------------------------------------------------
%% @doc
%% Join an already connected user to a room
%%
%% @spec join(RoomPid, SenderPid, SenderName) -> noreply
%% @end
%%--------------------------------------------------------------------
join(RoomPid, SenderPid, SenderName, FromRoomName) ->
  gen_server:cast(RoomPid, {join, SenderName, SenderPid, FromRoomName}).

%%--------------------------------------------------------------------
%% @doc
%% Remove a user from a room
%%
%% @spec join(RoomPid, SenderPid, SenderName) -> noreply
%% @end
%%--------------------------------------------------------------------
leave(RoomPid, SenderPid, SenderName, ToRoomName) ->
  gen_server:cast(RoomPid, {leave, SenderName, SenderPid, ToRoomName}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ServerName, Owner]) ->
    {ok, #state{roomid=ServerName, owner=Owner}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({chat_message, SenderName, Message}, State) ->
  OutMsg = #{type => message, identity => SenderName, message => Message},
  send_all(State#state.users, OutMsg),
  {noreply, State};

handle_cast({new_join, SenderName, SenderPid}, State) ->
  NewState = add_user(State, SenderPid),
  NewIdMsg = #{type => newidentity, identity => SenderName, former => <<>>},
  JoinMsg  = #{type => roomchange, identity => SenderName,
               roomid => NewState#state.roomid, former => <<>>},
  send_all(NewState#state.users, NewIdMsg),
  send_all(NewState#state.users, JoinMsg),
  {noreply, NewState};

handle_cast({join, SenderName, SenderPid, FromRoomName}, State) ->
  NewState = add_user(State, SenderPid),
  Msg      = #{type => roomchange, identity => SenderName,
               roomid => NewState#state.roomid, former => FromRoomName},
  send_all(NewState#state.users, Msg),
  {noreply, NewState};

handle_cast({leave, SenderName, SenderPid, ToRoomName}, State) ->
  NewState = remove_user(State, SenderPid),
  Msg      = #{type => roomchange, identity => SenderName,
               roomid => ToRoomName, former => State#state.roomid},
  send_all(NewState#state.users, Msg),
  {noreply, NewState};


handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send a message to all users in the room
%%
%% @spec send_all(UserMap, Message) -> term()
%% @end
%%--------------------------------------------------------------------
send_all(UserMap, Message) ->
    SendOne = fun (UPid, _) ->
                  chat_server_user_out:send_message(UPid, Message)
              end,
    maps:map(SendOne, UserMap).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add a new user to the room
%%
%% @spec add_user(State, UserPid) -> #state{}
%% @end
%%--------------------------------------------------------------------
add_user(State, UserPid) ->
  NewUsers = maps:put(UserPid, true, State#state.users),
  State#state{users = NewUsers}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Remove a user from the room
%%
%% @spec remove_user(State, UserPid) -> #state{}
%% @end
%%--------------------------------------------------------------------
remove_user(State, UserPid) ->
  NewUsers = maps:remove(UserPid, State#state.users),
  State#state{users = NewUsers}.
