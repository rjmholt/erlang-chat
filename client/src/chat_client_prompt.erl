-module(chat_client_prompt).

-author('Robert Holt').

prompt(Name, Room) ->
    Input = io:get_line(<<"[",Room/utf8,"] ",Name/utf8,"> ">>),
    parse_input(Input).

parse_input(Input) ->
    case string:substr(Input, 1, 1) of
        "#" ->
            do_command(Input);
        _ ->
            #{type => message, content => Input}
    end.

do_command(Line) ->
    CmdString = string:substr(Line, 2),
    [Cmd|Tkns] = string:tokens(CmdString, " "),
    case Cmd of
        "join" ->
            [RoomID] = Tkns,
            #{type => join, roomid => RoomID};
        "delete" ->
            [RoomID] = Tkns,
            #{type => delete, roomid => RoomID};
        "kick" ->
            [RoomID, Time, Identity] = Tkns,
            #{type => kick, roomid => RoomID,
              time => string:to_integer(Time),
              identity => Identity};
        "identitychange" ->
            [Identity] = Tkns,
            #{type => identitychange,
              identity => Identity};
        "createroom" ->
            [Identity, RoomID] = Tkns,
            #{type => createroom,
              roomid => RoomID,
              identity => Identity};
        "who" ->
            [RoomID] = Tkns,
            #{type => who, roomid => RoomID};
        "list" ->
            [] = Tkns,
            #{type => list};
        "quit" ->
            [] = Tkns,
            #{type => quit};
        _ ->
            #{type => message, content => Line}
    end.

