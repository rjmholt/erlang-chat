-module(chat_client_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  Addr = "localhost",
  Port = 7777,
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Addr, Port]).

init([Addr, Port]) ->
  SupFlags = #{strategy  => one_for_one,
               intensity => 1,
               period    => 5},
  Children = [
              #{id       => chat_client,
                start    => {chat_client_main, start_link, [Addr, Port]},
                restart  => permanent,
                shutdown => brutal_kill,
                type     => worker,
                modules  => []}
             ],
  {ok, {SupFlags, Children}}.
