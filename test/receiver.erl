-module(receiver).

-author('Robert Holt').

-export([echo_recv/0]).

echo_recv() ->
    receive
        Msg ->
            erlang:display(Msg)
    end,
    echo_recv().
