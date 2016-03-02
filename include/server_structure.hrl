% Server data structures
-record(client, {pid, name, room='', owns=[]}).
-record(room, {name, owner,
               occupants=ets:new(clients, [{keypos, #client.pid}]),
               bans=[]}).
-record(state, {rooms=ets:new(rooms, [{keypos, #room.name}]),
                clients=ets:new(clients, [{keypos, #client.pid}]),
                names=ets:new(names, [])}).
-record(ban, {identity,time}).

% Server entry room/base room
-define(MAINHALL, 'MainHall').
