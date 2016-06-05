-module(chat_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, []) ->
  Port = 7777,
  erlang:display("******** HELLO *********"),
  io:format("App start() called~n"),
  chat_server_sup:start_link(Port).

stop(_State) ->
  ok.
