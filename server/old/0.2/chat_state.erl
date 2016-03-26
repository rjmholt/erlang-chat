-module(chat_state).
-author('Robert Holt').

-include("include/server_structure.hrl").

-export([initialise/0,
         add_table/3,
         get_table/2]).

-record(state, {users, names, rooms}).

initialise() ->
    State = #state{},
    State1 = chat_user:initialise(State),
    chat_room:initialise(State1).

add_table(State, Tab, TabName) ->
    case TabName of
        users ->
            State#state{users=Tab};
        names ->
            State#state{names=Tab};
        rooms ->
            State#state{rooms=Tab}
    end.

get_table(State, TabName) ->
    case TabName of
        users ->
            State#state.users;
        names ->
            State#state.names;
        rooms ->
            State#state.rooms
    end.
