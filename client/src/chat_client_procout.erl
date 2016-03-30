-module(chat_client_procout).

-author('Robert Holt').

-include("include/client_structure.hrl").

-behaviour(gen_server).

-export([start/1, stop/0, send/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

start(Pid) ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE, [Pid], []).

stop() ->
    gen_server:call(?MODULE, stop).

send(Msg) ->
    gen_server:cast(?MODULE, {chat, Msg}).

init([Pid]) ->
    {ok, Pid}.

handle_info(_Info, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> init:stop().

handle_call(stop, _From, State) -> {stop, normal, stopped, State}.

handle_cast({chat, Msg}, Pid) ->
    erlang:display(Msg),
    Pid ! {self(), Msg},
    {noreply, Pid}.
