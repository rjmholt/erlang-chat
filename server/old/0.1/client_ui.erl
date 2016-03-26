-module(client_ui).
-export([start/2]).

-define(PS1, "> ").

start(Host, Port) ->
    % Beginning receiving processes
    Screen_Pid = spawn_link(fun () -> listen() end),
    State_Pid = spawn_link(fun () -> client_state:start(Screen_Pid) end),
    spawn_link(fun () -> client_in:start(State_Pid) end),
    % Now start connection to server
    Out_Conn_Pid = spawn_link(fun () -> client_out:start(Host, Port) end),
    loop(Out_Conn_Pid, State_Pid).

loop(Conn_Pid, State_Pid) ->
    Ln = take_input(State_Pid),
    Msg = parse_ln(Ln),
    Conn_Pid ! Msg,
    loop(Conn_Pid, State_Pid).

listen() ->
    receive
        {message, Msg, Sender} ->
            io:format("~p: ~p~n", [Sender, Msg])
    end,
    listen().

take_input(State_Pid) ->
    State_Pid ! {self(), get},
    receive
        {Name, Room} ->
            io:get_line(["[", Room, "] ", Name, " ", ?PS1])
    end.

parse_ln(Ln) ->
    case hd(Ln) of
        '#' ->
            client_command:mux(string:substr(Ln,1));
        _ ->
            {message, Ln}
    end.
