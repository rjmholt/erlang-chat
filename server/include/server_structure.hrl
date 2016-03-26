% Server data structures
%-record(client, {pid, name, room='', owns=[]}).
%-record(name_cpid, {name, cpid}).
%-record(room, {name, owner,
%               occupants=ets:new(clients, [{keypos, #client.pid}]),
%               bans=[]}).
%-record(state, {rooms=ets:new(rooms, [{keypos, #room.name}]),
%                clients=ets:new(clients, [{keypos, #client.pid}]),
%                names=ets:new(names, [])}).

-define(NODE, robchat).
% Central server name
-define(SERVER, chat_server).
% Name server
-define(NAMESERVER, nameserver).
% Server entry room/base room
-define(MAINHALL, <<"MainHall"/utf8>>).
