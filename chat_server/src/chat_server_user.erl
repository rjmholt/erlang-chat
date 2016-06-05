-module(chat_server_user).

-include("include/chat.hrl").

-export([start/1]).

-record(state, {name, room, sender}).

start(Socket) ->
    {ok, SendPid} = chat_server_user_out:start_link(Socket),
    {ok, Name} = chat_server_nameservice:new_user(),
    {ok, MainHall} = chat_server_nameservice:get_room(?MAINHALL),
    chat_server_room:new_join(MainHall, SendPid, Name),
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
    chat_server_room:chat_message(State#state.room, State#state.name, Msg).
