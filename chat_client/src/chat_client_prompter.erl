-module(chat_client_prompter).

-export([start_link/4]).

-record(state, {name, room}).

start_link(Socket, RecvPid, Name, Room) ->
  spawn_link(fun () ->
                 loop(#state{name = Name, room = Room}, RecvPid, Socket)
             end).

loop(State, RecvPid, Socket) ->
  NewState = receive
               {newstate, RecvPid, Name, Room} ->
                 #state{name = Name, room = Room}
             after
               0 -> State
             end,
  case prompt_user(NewState) of
    {message, OutMsg} ->
      OutBin = jiffy:encode(OutMsg),
      erlang:display(OutBin),
      gen_tcp:send(Socket, OutBin);
    Other ->
      erlang:display("Non-message out"),
      erlang:display(Other),
      ok
  end,
  loop(NewState, RecvPid, Socket).

prompt_user(State) ->
  Room = State#state.room,
  Name = State#state.name,
  Input = io:get_line(<<"[", Room/binary, "] ", Name/binary, "> ">>),
  io:format("*** INPUT ***: ~p~n", [Input]),
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
          erlang:display("message created"),
          #{type => message, message => list_to_binary(Input)}
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
