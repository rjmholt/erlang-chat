-module(chat_server_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Port) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
  io:format("Supervisor init() called~n"),
  SupFlags = #{strategy  => one_for_one,
               intensity => 1,
               period    => 5},
  Children = [
              #{id       => nameservice,
                start    => {chat_server_nameservice, start_link, []},
                restart  => permanent,
                shutdown => brutal_kill,
                type     => worker},

              #{id       => listener,
                start    => {chat_server_listener, start_link, [Port]},
                restart  => permanent,
                shutdown => brutal_kill,
                type     => worker}
             ],
  {ok, {SupFlags, Children}}.
