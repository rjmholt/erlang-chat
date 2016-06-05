-module(chat_server_listener).

-author('Robert Holt').

-export([start_link/1]).

start_link(Port) ->
  Pid = spawn_link(fun () -> init(Port) end),
  io:format("Listener started~n"),
  {ok, Pid}.

init(Port) ->
    case gen_tcp:listen(Port, [binary, {reuseaddr, true}]) of
      {ok, ListenSocket} ->
        par_connect(ListenSocket);
      {error, Reason} ->
        io:format("Unable to open listening socket:~n~p~n", [Reason])
    end.

par_connect(ListenSocket) ->
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} ->
      spawn(fun () -> par_connect(ListenSocket) end),
      chat_server_user:start(Socket);
    {error, Reason} ->
      io:format("Accepting socket failed:~n~p~n", [Reason])
  end.
