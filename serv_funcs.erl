-module(serv_funcs).
-author('Robert Holt').

-include("messages.hrl").
-include("server_structure.hrl").

-export([send/2, new_guest/1, remove_owner/2]).

% Sends Msg to all clients in the ets table Clients
send(Clients, Msg) ->
    SendEach = fun (#client{pid=CPid}, _) ->
                       CPid ! Msg
               end,
    ets:foldl(SendEach, not_used, Clients).

remove_owner(Rms, RoomTab) ->
    RmvOwner = fun (Rm) ->
                       ets:update_element(RoomTab, Rm, {#room.owner, ''})
               end,
    lists:map(RmvOwner, Rms).

new_guest(ClientTab) ->
    Nums = ets:foldl(fun get_guest_num_list/2, [], ClientTab),
    SortedNums = lists:sort(Nums),
    NewGuestNum = lowest_not_in_list(SortedNums),
    "guest" ++ integer_to_list(NewGuestNum).

lowest_not_in_list(Nums) ->
    lowest_acc(Nums, 1).
lowest_acc([N|Ns], Acc) ->
    if
        Acc < N -> Acc;
        true    -> lowest_acc(Ns, Acc+1)
    end;
lowest_acc([], Acc) -> Acc.

get_guest_num_list({_From, Client}, NumList) ->
    Name = Client#client.name,
    RE = "guest\\d+",
    case re:run(Name, RE, [{capture, none}]) of
        nomatch ->
            NumList;
        match ->
            % Remove "guest" from "guest\\d+" and turn it into a number
            Num = list_to_integer(string:substr(Name,6)),
            [Num|NumList]
    end.
