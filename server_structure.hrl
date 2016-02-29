% Server data structures
-record(client, {pid, name, room='', owns=[]}).
-record(room, {name, owner, occupants=ets:new(clients, [{keypos, #client.pid}])}).
-record(state, {rooms=ets:new(rooms, [{keypos, #room.name}]),
                clients=ets:new(clients, [{keypos, #client.pid}])}).
