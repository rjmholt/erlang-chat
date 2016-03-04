-module(client_state).
-export([start/1]).

start(Screen_Pid) ->
    Name = receive
               {newidentity, Ident, ""} -> Ident
           end,
    Room = receive
               {roomchange, MainHall, ""} -> MainHall
           end,
    State = {Name, Room},
    loop(Screen_Pid, State).

loop(Screen_Pid, State) ->
    {Name, Room} = State,
    receive
        % Messages from the outside world
        {message, Msg, Sender} ->
            Screen_Pid ! {message, Msg, Sender},
            loop(Screen_Pid, State);
        {newidentity, Identity, Former} ->
            Screen_Pid ! {newidentity, Identity, Former},
            if Former == Name -> loop(Screen_Pid, {Identity, Room});
               true           -> loop(Screen_Pid, State)
            end;
        {roomchange, Identity, New_Room, Former} ->
            Screen_Pid ! {roomchange, Identity, New_Room, Former},
            if Identity == Name -> loop(Screen_Pid, {Name, New_Room});
               true             -> loop(Screen_Pid, State)
            end;
        {roomlist, Room_List} ->
            Screen_Pid ! {roomlist, Room_List},
            loop(Screen_Pid, State);
        {roomcontents, Idents, Owner} ->
            Screen_Pid ! {roomcontents, Idents, Owner},
            loop(Screen_Pid, State);
        % Internal messages for state retrieval
        {Prompt_Pid, get} ->
            Prompt_Pid ! State,
            loop(Screen_Pid, State)
    end.

