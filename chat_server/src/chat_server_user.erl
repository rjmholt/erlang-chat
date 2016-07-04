-module(chat_server_user).

-include("include/chat.hrl").

-export([start/1]).

-record(state, {name, room, sender, socket}).

start(Socket) ->
  {ok, SendPid} = chat_server_user_out:start_link(Socket),
  {ok, Name} = chat_server_nameservice:new_user(),
  {ok, MainHall} = chat_server_nameservice:get_room(?MAINHALL),
  chat_server_room:new_join(MainHall, SendPid, Name),
  State = #state{name = Name, room = MainHall,
                 sender = SendPid, socket = Socket},
  loop(State).

loop(State) ->
  Socket = State#state.socket,
  NextState = receive
    {tcp, Socket, Bin} ->
      io:format("Received message:~n"),
      erlang:display(Bin),
      Msg = jiffy:decode(Bin, [return_maps]),
      process_message(State, Msg)
  end,
  case NextState of
    terminate ->
     terminate;
   _ -> 
      loop(NextState)
  end.

process_message(State,
                #{<<"type">>    := <<"message">>,
                  <<"message">> := Msg}) ->
  message(State, Msg);

process_message(State,
                #{<<"type">>     := <<"identitychange">>,
                  <<"identity">> := Identity}) ->
  identity_change(State, Identity);

process_message(State,
                #{<<"type">>   := <<"join">>,
                  <<"roomid">> := RoomID}) ->
  join_room(State, RoomID);

process_message(State,
                #{<<"type">>   := <<"createroom">>,
                  <<"roomid">> := RoomID}) ->
  create_room(State, RoomID);

process_message(State,
                #{<<"type">>   := <<"delete">>,
                  <<"roomid">> := RoomID}) ->
  delete_room(State, RoomID);

process_message(State, #{<<"type">> := <<"quit">>}) ->
  quit(State);

process_message(State, #{<<"type">> := <<"list">>}) ->
  room_list(State);

process_message(State,
                #{<<"type">>   := <<"who">>,
                  <<"roomid">> := RoomID}) ->
  room_contents(State, RoomID);

process_message(State,
                #{<<"type">>     := <<"kick">>,
                  <<"roomid">>   := RoomID,
                  <<"time">>     := Time,
                  <<"identity">> := Identity}) ->
  kick_from_room(State, RoomID, Time, Identity);

process_message(State, Msg) ->
  unknown_message_error(State, Msg).

message(State, Msg) ->
  chat_server_room:chat_message(State#state.room, State#state.name, Msg),
  State.

identity_change(State, NewName) ->
  case chat_server_nameservice:user_exists(NewName) of
    true ->
      Reply = #{type     => newidentity,
                identity => State#state.name,
                former   => State#state.name},
      chat_server_user_out:send_message(State#state.sender, Reply),
      State;
    _ ->
      Reply = #{type     => newidentity,
                identity => NewName,
                former   => State#state.name},
      chat_server_nameservice:change_user_name(State#state.name, NewName),
      chat_server_room:broadcast(State#state.room, Reply),
      State#state{name = NewName}
  end.

join_room(State, NewRoom) ->
  CurrRoomID = chat_server_room:get_name(State#state.room),
  case chat_server_nameservice:room_exists(NewRoom) of
    false ->
      Reply = #{type   => roomchange,
                roomid => CurrRoomID,
                former => CurrRoomID},
      chat_server_user_out:send_message(State#state.sender, Reply),
      State;
    _ ->
      OutMsg = #{type   => roomchange,
                 roomid => NewRoom,
                 former => CurrRoomID},
      {ok, NewRoomPid} = chat_server_nameservice:get_room(NewRoom),
      chat_server_room:leave(State#state.room),
      chat_server_room:join(NewRoomPid),
      chat_server_room:broadcast(State#state.room, OutMsg),
      chat_server_room:broadcast(NewRoomPid, OutMsg),
      State#state{room = NewRoomPid}
  end.

create_room(State, RoomName) ->
  case chat_server_nameservice:room_exists(State, RoomName) of
    true ->
      room_not_created;
    _ ->
      % TODO Record RoomPid under owned so that we can set room owners on quit
      {ok, _RoomPid} = chat_server_room:new(RoomName, State#state.name)
  end,
  RoomList = chat_server_nameservice:get_room_list(),
  Reply = #{type => roomlist, rooms => RoomList},
  chat_server_user_out:send_message(State#state.sender, Reply),
  State.

delete_room(State, RoomName) ->
  RoomPid = chat_server_nameservice:get_room(RoomName),
  case chat_server_room:is_owner(RoomPid, State#state.name) of
    false ->
      {ok, RoomList} = chat_server_nameservice:get_room_list(),
      Reply = #{type => roomlist, rooms => RoomList},
      chat_server_user_out:send_message(State#state.sender, Reply),
      State;
    _ ->
      CurrRoom = State#state.room,
      NextState = case CurrRoom = RoomPid of
        false ->
          State;
        _ ->
          JoinMsg = #{type     => roomchange,
                      identity => State#state.name,
                      roomid   => ?MAINHALL,
                      former   => RoomName},
          MainHall = chat_server_nameservice:get_room(?MAINHALL),
          chat_server_room:join(MainHall),
          chat_server_room:leave(State#state.room),
          chat_server_room:broadcast(State#state.room, JoinMsg),
          chat_server_room:broadcast(MainHall, JoinMsg),
          State#state{room = MainHall}
      end,
      chat_server_room:delete(RoomPid),
      NextState
  end.


quit(State) ->
  RoomName = chat_server_room:get_name(State#state.room),
  LeaveMsg = #{type => roomchange, roomid => <<>>, former => RoomName},
  chat_server_room:send_all(State#state.room, LeaveMsg),
  chat_server_room:leave(State#state.room),
  chat_server_nameservice:remove_user(self()),
  terminate.

room_list(State) ->
  RoomList = chat_server_nameservice:get_room_list(),
  Reply = #{type => roomlist, rooms => RoomList},
  chat_server_user_out:send_message(State#state.sender, Reply),
  State.

room_contents(State, RoomName) ->
  RoomPid = chat_server_nameservice:get_room(RoomName),
  {OccupantList, Owner} = chat_server_room:get_contents(RoomPid),
  Reply = #{type => roomcontents, identities => OccupantList, owner => Owner},
  chat_server_user_out:send_message(State#state.sender, Reply),
  State.

kick_from_room(State, RoomName, Time, KickedName) ->
  erlang:display("Kick message ignored"),
  State.

unknown_message_error(State, Msg) ->
  Reply = #{type => error, reason => unknown_message, message => Msg},
  chat_server_user_out:send_message(State#state.sender, Reply),
  State.
