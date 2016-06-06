-module(chat_client_receiver).

-export([start_link/2]).

-record(state, {name, room}).

start_link(Addr, Port) ->
  Pid = spawn_link(fun () -> init(Addr, Port) end),
  {ok, Pid}.

init(Addr, Port) ->
  case gen_tcp:connect(Addr, Port, []) of
    {ok, Socket} ->
      {Name, Room} = connect_handshake(Socket),
      PromptPid = chat_client_prompter:start_link(Socket, self(), Name, Room),
      receive_loop(Socket, PromptPid, #state{name = Name, room = Room});
    {error, Reason} ->
      io:format("~nCannot connect to server.~nReason: ~p~n", [Reason])
  end.

receive_loop(Socket, PromptPid, State) ->
  NewState = receive
               {tcp, Socket, InBin} ->
                 InMsg = jiffy:decode(InBin, [return_maps]),
                 NextState = process_received_message(State, InMsg),
                 NextState
             end,
  PromptPid ! {newstate, self(), NewState#state.name, NewState#state.room},
  receive_loop(Socket, PromptPid, NewState).

process_received_message(State,
                #{<<"type">>     := <<"message">>,
                  <<"identity">> := Sender,
                  <<"message">>  := Content}) ->
  io:format("~s: ~s~n", [Sender, Content]),
  State.

connect_handshake(Socket) ->
  Name = receive
           {tcp, Socket, NameBin} ->
             NameMsg = jiffy:decode(NameBin, [return_maps]),
             #{<<"type">>     := <<"newidentity">>,
               <<"identity">> := Ident,
               <<"former">>   := <<>>} = NameMsg,
             Ident
         end,
  Room = receive
           {tcp, Socket, RoomBin} ->
             RoomMsg = jiffy:decode(RoomBin, [return_maps]),
             #{<<"type">>     := <<"roomchange">>,
               <<"identity">> := Name,
               <<"former">>   := <<>>,
               <<"roomid">>   := RoomID} = RoomMsg,
             RoomID
         end,
  {Name, Room}.

