-module(chat_client_prompt).

-author('Robert Holt').

-define(PROMPT, chat_prompt).

-export([start/2]).

start(Name, Room) ->
    prompt(Name, Room).

prompt(Name, Room) ->
    {NewName, NewRoom} = receive
                             {update, Nm, Rm} ->
                                 {Nm, Rm}
                         after
                             1 ->
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

