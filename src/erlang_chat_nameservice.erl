%%%-------------------------------------------------------------------
%%% @author rob
%%% @copyright (C) 2016, rob
%%% @doc
%%%
%%% @end
%%% Created : 2016-06-03 18:55:42.370790
%%%-------------------------------------------------------------------
-module(erlang_chat_nameservice).

-behaviour(gen_server).

%% API
-export([start_link/0,
         new_user/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(MAINHALL, <<"MainHall">>).

% The globally required details of a room
-record(room, {name, pid, users=#{}}).
% The globally required details of a user
-record(user, {pid, name}).
% The nameserver's state
-record(state, {users=ets:new(users, [{keypos, #user.name}]),
                rooms=ets:new(rooms, [{keypos, #room.name}])}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Add a new user to the server and give them a name automatically
%%
%% @spec new_user() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
new_user() ->
    gen_server:call(?SERVER, new_user).

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
init([]) ->
    State    = #state{},
    {ok, MainHall} = erlang_chat_room:new(?MAINHALL),
    MH_Entry = #room{name=?MAINHALL, pid=MainHall},
    ets:insert(State#state.rooms, MH_Entry),
    {ok, State}.

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
handle_call(new_user, UPid, State) ->
    Name = generate_new_username(State#state.users),
    User = #user{pid = UPid, name = Name},
    NewState = add_user(State, User),
    {reply, {ok, Name}, NewState};

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
%% Generate a new guest username, based on the usernames already in use
%%
%% @spec generate_new_username(UserTable) -> binary()
%% @end
%%--------------------------------------------------------------------
generate_new_username(UserTable) ->
    Num = get_lowest_unused_guest_num(UserTable),
    NumBin = list_to_binary(integer_to_list(Num)),
    <<"guest", NumBin>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Find the lowest integer not used in a username of the form "guest<num>"
%%
%% @spec get_lowest_unused_guest_num(UserTable) -> integer()
%% @end
%%--------------------------------------------------------------------
get_lowest_unused_guest_num(UserTable) ->
    GuestNums = ets:foldl(fun collect_guest_num/2, [], UserTable),
    lowest_not_in_list(GuestNums).

collect_guest_num(UserEntry, NumList) ->
    Name = UserEntry#user.name,
    case Name of
        <<"guest", NumBin/binary>> ->
            Num = list_to_integer(binary_to_list(NumBin)),
            [Num | NumList];
        _ ->
            NumList
    end.

lowest_not_in_list(List) ->
    SeenArray = array:new(length(List), {default, unseen}),
    lowest_acc(List, SeenArray).

lowest_acc([Num|Ns], SeenArray) ->
    SeenArray1 = case Num-1 < array:size(SeenArray) of
                     false ->
                         SeenArray;
                     true ->
                        array:set(Num-1, seen, SeenArray)
                 end,
    lowest_acc(Ns, SeenArray1);

lowest_acc([], SeenArray) ->
    lowest_zero_index(0, SeenArray).

lowest_zero_index(Idx, SeenArray) ->
    case Idx >= array:size(SeenArray) of
        true  -> Idx+1;
        false ->
            case array:get(Idx, SeenArray) of
                unseen -> Idx+1;
                seen   ->
                    lowest_zero_index(Idx+1, SeenArray)
            end
    end.
