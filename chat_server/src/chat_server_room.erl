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
         broadcast/2,
         add/2,
         remove/2,
         delete/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("include/chat.hrl").

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
new(RoomID, Owner) ->
    gen_server:start_link(?MODULE, [RoomID, Owner], []).

%%--------------------------------------------------------------------
%% @doc
%% Sends a message to all users in the room
%%
%% @spec broadcast(RoomPid, Message) -> noreply
%% @end
%%--------------------------------------------------------------------
broadcast(RoomPid, Message) ->
    gen_server:cast(RoomPid, {broadcast, Message}).

%%--------------------------------------------------------------------
%% @doc
%% Add the sending user to the room
%%
%% @spec add(RoomPid, UPid) -> ok
%% @end
%%--------------------------------------------------------------------
add(RoomPid, UPid) ->
  gen_server:call(RoomPid, {add, UPid}).

%%--------------------------------------------------------------------
%% @doc
%% Remove the sending user from the room
%%
%% @spec remove(RoomPid, UPid) -> ok
%% @end
%%--------------------------------------------------------------------
remove(RoomPid, UPid) ->
  gen_server:call(RoomPid, {remove, UPid}).

%%--------------------------------------------------------------------
%% @doc
%% Send all room occupants to MainHall and terminate the room, if
%% the Owner is correct
%%
%% @spec delete(RoomPid, UPid) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
delete(RoomPid, UPid) ->
  gen_server:call(RoomPid, {delete, UPid}).

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
init([RoomID, OwnerName]) ->
    {ok, #state{roomid=RoomID, owner=OwnerName}}.

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
handle_call({add, UPid}, _From, State) ->
  NewState = add_user(State, UPid),
  {reply, ok, NewState};

handle_call({remove, UPid}, _From, State) ->
  NewState = remove_user(State, UPid),
  {reply, ok, NewState};

handle_call({delete, SenderName}, _From, State) ->
  MsgBody = #{type => roomchange,
              roomid => ?MAINHALL,
              former => State#state.roomid},
  MainHall = chat_server_nameservice:get_room(?MAINHALL),
  MsgEach = fun (UserPid) ->
                UserName = chat_server_user:get_name(UserPid),
                Msg = MsgBody#{identity => UserName},
                chat_server_user_out:send_message(UserPid, Msg)
  end,
  Users = maps:keys(State#state.users),
  chat_server_room:add(MainHall, Users),
  lists:map(MsgEach, Users),
  {stop, shutdown, ok, State};

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
handle_cast({broadcast, Msg}, State) ->
  send_all(State#state.users, Msg),
  {noreply, State};

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
