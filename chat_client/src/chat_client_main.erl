-module(chat_client_main).

-export([start_link/2]).

-record(state, {name, room}).

start_link(Addr, Port) ->
  Pid = spawn_link(fun () -> init(Addr, Port) end),
  {ok, Pid}.

init(Addr, Port) ->
  case gen_tcp:connect(Addr, Port, []) of
    {ok, Socket} ->
      {Name, Room}  = connect_handshake(Socket),
      loop(#state{name = Name, room = Room}, Socket);
    {error, Reason} ->
      io:format("~nCannot connect to server.~nReason: ~p~n", [Reason])
  end.

loop(State, Socket) ->
  NewState = receive
               {tcp, Socket, InBin} ->
                 InMsg = jiffy:decode(InBin, [return_maps]),
                 process_received_message(State, InMsg)
             after
               0 ->
                 State
             end,
  case prompt_user(NewState) of
    {message, OutMsg} ->
      OutBin = jiffy:encode(OutMsg),
      gen_tcp:send(Socket, OutBin);
    _ -> ok
  end,
  loop(NewState, Socket).

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

process_received_message(State,
                #{<<"type">>     := <<"message">>,
                  <<"identity">> := Sender,
                  <<"message">>  := Content}) ->
  io:format("~s: ~s~n", [Sender, Content]),
  State.

prompt_user(State) ->
  Room = State#state.room,
  Name = State#state.name,
  Input = io:get_line(<<"[", Room/utf8, "] ", Name/utf8, "> ">>),
  try parse_input(Input) of
      Msg ->
        {message, Msg}
  catch
      _ -> io:format("Bad command")
  end.

parse_input(Input) ->
    case string:substr(Input, 1, 1) of
        "#" ->
            extract_command(Input);
        _ ->
            #{type => message, content => list_to_binary(Input)}
    end.

extract_command(Line) ->
    CmdString = string:substr(Line, 2),
    [Cmd|Tkns] = string:tokens(CmdString, " "),
    case Cmd of
        "join" ->
            [RoomID] = Tkns,
            #{type => join, roomid => list_to_binary(RoomID)};
        "delete" ->
            [RoomID] = Tkns,
            #{type => delete, roomid => list_to_binary(RoomID)};
        "kick" ->
            [RoomID, Time, Identity] = Tkns,
            #{type => kick, roomid => list_to_binary(RoomID),
              time => string:to_integer(Time),
              identity => list_to_binary(Identity)};
        "identitychange" ->
            [Identity] = Tkns,
            #{type => identitychange,
              identity => list_to_binary(Identity)};
        "createroom" ->
            [Identity, RoomID] = Tkns,
            #{type => createroom,
              roomid => list_to_binary(RoomID),
              identity => list_to_binary(Identity)};
        "who" ->
            [RoomID] = Tkns,
            #{type => who, roomid => list_to_binary(RoomID)};
        "list" ->
            [] = Tkns,
            #{type => list};
        "quit" ->
            [] = Tkns,
            #{type => quit};
        _ ->
            #{type => message, content => list_to_binary(Line)}
    end.
