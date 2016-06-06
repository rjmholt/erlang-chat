-module(chat_client_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(Addr, Port) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Addr, Port]).

init([_Addr, _Port]) ->
  SupFlags = #{strategy  => one_for_one,
               intensity => 1,
               period    => 5},
  Children = [
             ],
  {ok, {SupFlags, Children}}.
