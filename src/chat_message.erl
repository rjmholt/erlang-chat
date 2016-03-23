-module(chat_message).

-author('Robert Holt').

-include("include/messages.hrl").

% Convert incoming
% CLIENT => SERVER
-export([convert/1]).

% SERVER => CLIENT
-export([message/3,
         newidentity/3,
         roomchange/4,
         roomlist/2,
         roomcontents/4]).

% ERROR MESSAGES
-export([err/2]).

convert(#{<<"type">> := <<"join">>, <<"roomid">> := RoomID}) ->
    #join{roomid=RoomID};

convert(#{<<"type">> := <<"delete">>, <<"roomid">> := RoomID}) ->
    #delete{roomid=RoomID};

convert(#{<<"type">> := <<"kick">>,
          <<"roomid">> := RoomID,
          <<"time">> := Time,
          <<"identity">> := Identity}) ->
    #kick{roomid=RoomID,time=Time,identity=Identity};

convert(#{<<"type">> := <<"message">>, <<"content">> := Content}) ->
    #message{content=Content};

convert(#{<<"type">> := <<"identitychange">>, <<"identity">> := Identity}) ->
    #identitychange{identity=Identity};

convert(#{<<"type">> := <<"createroom">>,
          <<"identity">> := Identity,
          <<"roomid">> := RoomID}) ->
    #createroom{identity=Identity,roomid=RoomID};

convert(#{<<"type">> := <<"who">>, <<"roomid">> := RoomID}) ->
    #who{roomid=RoomID};

convert(#{<<"type">> := <<"list">>}) -> list;

convert(#{<<"type">> := <<"quit">>}) -> quit.

message(Pid, Content, Identity) ->
    {chat, Pid,
     #{type => message,
      content => Content,
      identity => Identity}}.

newidentity(Pid, Identity, Former) ->
    {chat, Pid,
     #{type => newidentity,
      identity => Identity,
      former => Former}}.

roomchange(Pid, Identity, RoomID, Former) ->
    {chat, Pid,
     #{type => roomchange,
      identity => Identity,
      roomid => RoomID,
      former => Former}}.

roomlist(Pid, Rooms) ->
    {chat, Pid,
     #{type => roomlist,
      rooms => Rooms}}.

roomcontents(Pid, RoomID, Owner, Identities) ->
    {chat, Pid,
     #{type => roomcontents,
      roomid => RoomID,
      owner => Owner,
      identities => Identities}}.

err(Pid, Msg) ->
    {chat, Pid,
     #{type => error,
      message => Msg}}.
