-module(chat_client_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Addr = "localhost",
  Port = 7777,
  chat_client_receiver:start_link(Addr, Port).

stop(_State) ->
  ok.
