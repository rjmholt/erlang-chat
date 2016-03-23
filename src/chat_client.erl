-module(chat_client).

-author('Robert Holt').

-export([start/2]).

start(HostName, Port) ->
    Connection = chat_client_connection:connect(self(), HostName, Port),
    {Name, Room} = await_welcome(Connection),
    loop(Connection, Name, Room).

loop(Connection, PrevName, PrevRoom) ->
    {Name, Room} = receive
        {Connection, Msg} ->
                           operate(PrevName, PrevRoom, Msg)
                   after 0 ->
                           {PrevName, PrevRoom}
                   end,
    try 
        prompt(Name, Room)
    of
        Resp ->
            Connection ! Resp
    catch
        _ ->
            io:format("~p~n", ["Invalid message"])
    end,
    loop(Connection, Name, Room).

await_welcome(Connection) ->
    Name = receive 
               {Connection, #{<<"type">> := <<"newidentity">>,
                       <<"identity">> := ID,
                       <<"former">> := <<>>}} -> ID
           end,
    Room = receive
               {Connection, #{<<"type">> := <<"roomchange">>,
                              <<"roomid">> := Rm,
                              <<"former">> := <<>>}} -> Rm
           end,
    {Name, Room}.

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

operate(PrevName, PrevRoom, Msg) ->
    case Msg of
        #{<<"type">> := <<"message">>,
         <<"content">> := Content,
         <<"identity">> := Identity} ->
            io:format("~s: ~p~n", [Identity, Content]),
            {PrevName, PrevRoom};
        #{<<"type">> := <<"newidentity">>,
          <<"identity">> := Identity,
          <<"former">> := Former} ->
            if
                Identity =:= Former ->
                    io:format("Invalid identitychange command", []),
                    {PrevName, PrevRoom};
                Former =:= PrevName ->
                    io:format("~s is now ~s~n", [Former, Identity]),
                    {Identity, PrevRoom};
                true ->
                    io:format("~s is now ~s~n", [Former, Identity]),
                    {PrevName, PrevRoom}
            end;
        #{<<"type">> := <<"roomchange">>,
          <<"identity">> := Identity,
          <<"roomid">> := RoomID,
          <<"former">> := Former} ->
            if
                RoomID =:= Former ->
                    io:format("Invalid roomchange command", []),
                    {PrevName, PrevRoom};
                Identity =:= PrevName ->
                    io:format("~s moved from ~s to ~s~n",
                              [Identity, Former, RoomID]),
                    {PrevName, RoomID};
                true ->
                    io:format("~s moved from ~s to ~s~n",
                              [Identity, Former, RoomID]),
                    {PrevName, PrevRoom}
            end;
        #{<<"type">> := <<"roomlist">>,
          <<"rooms">> := Rooms} ->
            print_roomlist(Rooms),
            {PrevName, PrevRoom};
        #{<<"type">> := <<"roomcontents">>,
          <<"roomid">> := RoomID,
          <<"owner">> := Owner,
          <<"identities">> := Identities} ->
            print_roomcontents(RoomID, Owner, Identities),
            {PrevName, PrevRoom};
        #{<<"type">> := <<"error">>,
          <<"message">> := Msg} ->
            io:format("Server error: ~p~n", [Msg]),
            {PrevName, PrevRoom};
        Msg ->
            io:format("Unsupported message: ~p~n", [Msg]),
            {PrevName, PrevRoom}
    end.
