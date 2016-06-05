%%%-------------------------------------------------------------------
%%% @author rob
%%% @copyright (C) 2016, rob
%%% @doc
%%%
%%% @end
%%% Created : 2016-06-03 18:55:42.370790
%%%-------------------------------------------------------------------
-module(chat_server_nameservice).

-behaviour(gen_server).

%% API
-export([start_link/0,
         % User functions
         new_user/0,
         % Room functions
         get_room/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("include/chat.hrl").
-define(SERVER, ?MODULE).
-define(GUEST, "guest").

% The globally required details of a room
-record(room, {name, pid}).
% The globally required details of a user
-record(user, {name, pid}).
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

%%--------------------------------------------------------------------
%% @doc
%% Get the Pid of a room based on its name
%%
%% @spec get_room(RoomName) -> {ok, RoomPid} | no_such_room
%% @end
%%--------------------------------------------------------------------
get_room(RoomName) ->
    gen_server:call(?SERVER, {get_room, RoomName}).

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
    {ok, MainHall} = chat_server_room:new(?MAINHALL, <<>>),
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
handle_call(new_user, {UPid, _Tag}, State) ->
    Name = generate_new_username(State#state.users),
    User = #user{pid = UPid, name = Name},
    NewState = add_user(State, User),
    {reply, {ok, Name}, NewState};

handle_call({get_room, RoomName}, _From, State) ->
    Reply = get_room_pid(State#state.rooms, RoomName),
    {reply, Reply, State};

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

%%%-------------------------------------------------------------------
%%% Room functions
%%%-------------------------------------------------------------------

get_room_pid(RoomTable, RoomName) ->
    case ets:lookup(RoomTable, RoomName) of
        [#room{pid=Pid}] ->
            {ok, Pid};
        [] ->
            no_such_room
    end.

%%%-------------------------------------------------------------------
%%% User functions
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add a new user to the user name table
%%
%% @spec add_user(UserTable, UserRecord) -> boolean()
%% @end
%%--------------------------------------------------------------------
add_user(State, User) ->
    ets:insert_new(State#state.users, User),
    State.

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
    <<?GUEST, NumBin/binary>>.

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get a list of all numbers of users with names of the form "guest<num>"
%%
%% @spec collect_guest_num(UserEntry, NumList) -> [integer()]
%% @end
%%--------------------------------------------------------------------
collect_guest_num(UserEntry, NumList) ->
    Name = UserEntry#user.name,
    case Name of
        <<?GUEST, NumBin/binary>> ->
            Num = list_to_integer(binary_to_list(NumBin)),
            [Num | NumList];
        _ ->
            NumList
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Find the lowest non-zero integer not in a list of integers
%%
%% @spec lowest_not_in_list(List) -> integer()
%% @end
%%--------------------------------------------------------------------
lowest_not_in_list(List) ->
    SeenArray = array:new(length(List), {default, unseen}),
    lowest_acc(List, SeenArray).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Find the lowest non-zero integer not in a list of integers with an
%% accumulator
%%
%% @spec lowest_acc(List, SeenArray) -> integer()
%% @end
%%--------------------------------------------------------------------
lowest_acc([Num|Ns], SeenArray) ->
    SeenArray1 = case Num-1 < array:size(SeenArray) of
                     false ->
                         SeenArray;
                     true ->
                        array:set(Num-1, seen, SeenArray)
                 end,
    lowest_acc(Ns, SeenArray1);

lowest_acc([], SeenArray) ->
    1 + lowest_false_index(0, SeenArray).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finds the lowest index in an array of booleans which contains
%% false. If no such index exists, the length of the array is
%% returned.
%%
%% @spec lowest_false_index(Index, Array) -> integer()
%% @end
%%--------------------------------------------------------------------
lowest_false_index(Idx, SeenArray) ->
    case Idx >= array:size(SeenArray) of
        true  -> Idx;
        false ->
            case array:get(Idx, SeenArray) of
                unseen -> Idx;
                seen   ->
                    lowest_false_index(Idx, SeenArray)
            end
    end.
