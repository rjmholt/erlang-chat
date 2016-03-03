% Message protocol used by the chat server

% CLIENT => SERVER
-record(join, {roomid}).
-record(delete, {roomid}).
-record(kick, {roomid, time, identity}).
-record(message, {content}).
-record(identitychange, {identity}).
-record(createroom, {identity, roomid}).
-record(who, {roomid}).

% Singleton messages are:
% - list
% - quit


% SERVER => CLIENT
-record(serv_msg, {content, identity}).
-record(newidentity, {identity, former}).
-record(roomchange, {identity, roomid, former}).
-record(roomlist, {rooms}).
-record(roomcontents, {roomid, owner, identities}).

% Extra message, largely for debug
-record(error, {content}).

