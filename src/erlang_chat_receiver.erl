-module(erlang_chat_receiver).

-export([start/2]).

-record(state, {name, room}).

start(Socket, SendPid) ->
    {ok, Name} = 
