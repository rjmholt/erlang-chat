-module(chat_client_out).

-author('Robert Holt').

-include("include/client_structure.hrl").

-behaviour(gen_server).

-export([start/1, stop/0, send/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

start(Socket) ->
    gen_server:start_link({local, ?CONNECTION},
                          ?MODULE, [Socket], []).

stop() ->
    gen_server:call(?CONNECTION, stop).

send(Msg) ->
    gen_server:cast(?CONNECTION, {chat, Msg}).


init([Socket]) ->
    {ok, Socket}.

handle_info(_Info, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> init:stop().

handle_call(stop, _From, State) -> {stop, normal, stopped, State}.

handle_cast({chat, Msg}, Socket) ->
    Bin = jiffy:encode(Msg),
    gen_tcp:send(Socket, <<Bin/binary, "\n"/utf8>>),
    {noreply, Socket}.
