% Message protocol used by the chat server

-record(join, {roomid}).
-record(delete, {roomid}).
-record(kick, {roomid, time, identity}).
-record(message, {contents}).
-record(serv_msg, {content, identity}).
-record(identitychange, {identity}).
-record(roomchange, {identity, roomid, former}).
-record(roomcontents, {roomid, owner, identities}).
-record(roomlist, {rooms}).
-record(who, {roomid}).

% Singleton messages are:
% - list
% - quit
