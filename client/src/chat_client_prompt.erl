-module(chat_client_prompt).

-author('Robert Holt').

-define(PROMPT, chat_prompt).

-export([start/2]).

start(Name, Room) ->
    prompt(Name, Room).

prompt(Name, Room) ->
    io:format("Prompting~n"),
    {NewName, NewRoom} = receive
                             {update, Nm, Rm} ->
                                 {Nm, Rm}
                         after 0 ->
                                   {Name, Room}
                         end,
    Input = io:get_line(<<"[",NewRoom/binary,"] ",NewName/binary,"> ">>),
    try parse_input(Input) of
        Msg ->
            chat_client_out:send(Msg)
    catch
        _ -> io:format("Bad command")
    end,
    prompt(NewName, NewRoom).

parse_input(Input) ->
    case string:substr(Input, 1, 1) of
        "#" ->
            do_command(Input);
        _ ->
            #{type => message, content => list_to_binary(Input)}
    end.

do_command(Line) ->
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

