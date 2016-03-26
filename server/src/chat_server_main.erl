-module(chat_server_main).

-author('Robert Holt').

-include("include/server_structure.hrl").

-export([start/1, start_debug/0]).

start(Port) ->
    % TODO: Start supervisor rather than nameserver
    chat_server_nameserver:start(),
    RPid = chat_server_room:create_registered(mainhall, ?MAINHALL, <<>>),
    chat_server_nameserver:add_room(?MAINHALL, RPid),
    chat_server_connection:start(Port).

start_debug() ->
    chat_server_nameserver:start(),
    RPid = chat_server_room:create_registered(mainhall, ?MAINHALL, <<>>),
    rpc:call(node(), chat_server_nameserver, add_room, [?MAINHALL, RPid]).
