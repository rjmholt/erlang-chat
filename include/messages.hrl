% Message protocol used by the chat server

-record(join, {roomid}).
-record(delete, {roomid}).
-record(kick, {roomid, time, identity}).
-record(message, {content}).
-record(serv_msg, {content, identity}).
-record(identitychange, {identity}).
-record(newidentity, {identity, former}).
-record(roomchange, {identity, roomid, former}).
-record(createroom, {identity, roomid}).
-record(roomcontents, {roomid, owner, identities}).
-record(roomlist, {rooms}).
-record(who, {roomid}).

% Extra message, largely for debug
-record(error, {content}).

% Singleton messages are:
% - list
% - quit
