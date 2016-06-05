-module(chat_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, []) ->
  Port = 7777,
  chat_server_sup:start_link(Port).

stop(_State) ->
  ok.
