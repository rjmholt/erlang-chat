-module(chat_client_in).

-author('Robert Holt').

-include("include/client_structure.hrl").

-export([start/2]).

start(HostName, Port) ->
    {ok, Socket} = gen_tcp:connect(HostName, Port, [binary]),
    spawn_link(chat_client_out, start, [Socket]),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            Msg = jiffy:decode(Bin, [return_maps]),
            ?CLIENT ! Msg
    end,
    loop(Socket).
