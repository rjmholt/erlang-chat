-module(client_command).
-export([mux/1]).

mux(Ln) ->
    [Cmd | Tkns] = string:tokens(Ln, " "),
    case list_to_atom(Cmd) of
        join ->
            [Room] = Tkns,
            {join, Room};
        identitychange ->
            [Ident] = Tkns,
            {identitychange, Ident};
        who ->
            [Room] = Tkns,
            {who, Room};
        createroom ->
            [Room] = Tkns,
            {createroom, Room};
        delete ->
            [Room] = Tkns,
            {delete, Room};
        kick ->
            [Time, Name, Room] = Tkns,
            {kick, Room, Time, Name};
        list ->
            {list};
        quit ->
            {quit};
        logout ->
            {quit};
        _ ->
            {message, "#" ++ Ln}
    end.
