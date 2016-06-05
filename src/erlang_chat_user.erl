-module(erlang_chat_user).

-include("include/erlang_chat.hrl").

-export([start/1]).

-record(state, {name, room, sender}).

start(Socket) ->
    {ok, SendPid} = erlang_chat_user_out:start_link(Socket),
    {ok, Name} = erlang_chat_nameservice:new_user(),
    {ok, MainHall} = erlang_chat_nameservice:get_room(?MAINHALL),
    erlang_chat_room:new_join(MainHall, SendPid, Name),
    State = #state{name = Name, room = MainHall, sender = SendPid},
    loop(Socket, State).

loop(Socket, State) ->
    receive
        {tcp, Socket, Bin} ->
            Msg = jiffy:decode(Bin, [return_maps]),
            process_message(State, Msg)
    end,
    loop(Socket, State).

process_message(State,
                #{<<"type">> := <<"message">>,
                  <<"message">> := Msg}) ->
    erlang_chat_room:chat_message(State#state.room, State#state.name, Msg).
