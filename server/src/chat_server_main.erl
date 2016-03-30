-module(chat_server_main).

-author('Robert Holt').

-include("include/server_structure.hrl").

% Application API
-export([start/2, stop/1]).

-export([run/1, start_debug/0]).

start(normal, [Port]) -> run(Port);
start(normal, []) -> run().

stop(_) -> init:stop().

run(Port) ->
    % TODO: Start supervisor rather than nameserver
    chat_server_nameserver:start(),
    {ok, RPid} = chat_server_room:create_registered(mainhall, ?MAINHALL, <<>>),
    chat_server_nameserver:add_room(?MAINHALL, RPid),
    chat_server_listener:start(Port).

run() ->
    chat_server_nameserver:start(),
    RPid = chat_server_room:create_registered(mainhall, ?MAINHALL, <<>>),
    chat_server_nameserver:add_room(?MAINHALL, RPid),
    chat_server_listener:start().

start_debug() ->
    chat_server_nameserver:start(),
    RPid = chat_server_room:create_registered(mainhall, ?MAINHALL, <<>>),
    rpc:call(node(), chat_server_nameserver, add_room, [?MAINHALL, RPid]).
