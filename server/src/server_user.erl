-module(server_user).

-include("include/chat.hrl").

-export([start/1]).

-record(state, {name, room, sender}).

start(Socket) ->
    {ok, SendPid} = server_user_out:start_link(Socket),
    {ok, Name} = server_nameservice:new_user(),
    {ok, MainHall} = server_nameservice:get_room(?MAINHALL),
    server_room:new_join(MainHall, SendPid, Name),
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
    server_room:chat_message(State#state.room, State#state.name, Msg).
