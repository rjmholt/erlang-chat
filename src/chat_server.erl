-module(chat_server).
-author('Robert Holt').

-include("include/server_structure.hrl").
-include("include/messages.hrl").

% Server API
-export([start/0, stop/0, new_connection/1, handle_msg/2]).

% Callbacks for gen_server
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

% Server API definitions
start() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).
handle_msg(Pid, Msg) -> gen_server:cast(?MODULE, {Pid, Msg}).
new_connection(Pid) -> gen_server:cast(?MODULE, {Pid, new_connection}).

% ========== General server definitions =================
init([]) ->
    InitState = chat_state:initialise(),
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
    NewIdentMsg = #newidentity{identity=Name,former=''},
    From ! NewIdentMsg,
    chat_room:add_user(State, ?MAINHALL, From, Name),
    RmChgMsg = #roomchange{identity=Name,roomid=?MAINHALL,former=''}.

handle_cast({From, Msg}, State) -> ok;

handle_cast({From, Msg}, State) -> ok;

handle_cast({From, Msg}, State) -> ok;

handle_cast({From, Msg}, State) -> ok;

handle_cast({From, Msg}, State) -> ok;

handle_cast({From, Msg}, State) ->
    Reply = io_lib:format("~s: ~p~n", ["Unsupported message", Msg]),
    ReplyBin = unicode:characters_to_binary(Reply),
    From ! #error{message=ReplyBin},
    {noreply, State}.
