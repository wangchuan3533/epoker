-module(protocol).
-include("messages_pb.hrl").

-export([decode/1, encode/1]).

decode(Bin) ->
  Message = messages_pb:decode_message(Bin),
  case Message of
    #message{type = 'JOIN_TABLE_REQ', data = Data} ->
      messages_pb:decode_jointablereq(Data);
    #message{type = 'JOIN_TABLE_RES', data = Data} ->
      messages_pb:decode_jointableres(Data);
    #message{type = 'LEAVE_TABLE_REQ', data = Data} ->
      messages_pb:decode_leavetablereq(Data);
    #message{type = 'LEAVE_TABLE_RES', data = Data} ->
      messages_pb:decode_leavetableres(Data);
    #message{type = 'LIST_TABLE_REQ', data = Data} ->
      messages_pb:decode_listtablereq(Data);
    #message{type = 'LIST_TABLE_RES', data = Data} ->
      messages_pb:decode_listtableres(Data);
    #message{type = 'LEAVE_GAME_REQ', data = Data} ->
      messages_pb:decode_leavegamereq(Data);
    #message{type = 'LEAVE_GAME_RES', data = Data} ->
      messages_pb:decode_leavegameres(Data);
    #message{type = 'ACTION_REQ', data = Data} ->
      messages_pb:decode_actionreq(Data);
    #message{type = 'ACTION_RES', data = Data} ->
      messages_pb:decode_actionres(Data);
    #message{type = 'OTHER_JOIN_TABLE_NTF', data = Data} ->
      messages_pb:decode_otherjointablentf(Data);
    #message{type = 'OTHER_LEAVE_TABLE_NTF', data = Data} ->
      messages_pb:decode_otherleavetablentf(Data);
    #message{type = 'OTHER_ACTION_NTF', data = Data} ->
      messages_pb:decode_otheractionntf(Data);
    _Other ->
      error
  end.

encode(Msg) ->
  Encoded = messages_pb:encode(Msg),
  case Msg of
    #jointablereq{} ->
      messages_pb:encode(#message{type = 'JOIN_TABLE_REQ', data = Encoded});
    #jointableres{} ->
      messages_pb:encode(#message{type = 'JOIN_TABLE_RES', data = Encoded});
    #leavetablereq{} ->
      messages_pb:encode(#message{type = 'LEAVE_TABLE_REQ', data = Encoded});
    #leavetableres{} ->
      messages_pb:encode(#message{type = 'LEAVE_TABLE_RES', data = Encoded});
    #listtablereq{} ->
      messages_pb:encode(#message{type = 'LIST_TABLE_REQ', data = Encoded});
    #listtableres{} ->
      messages_pb:encode(#message{type = 'LIST_TABLE_RES', data = Encoded});
    #leavegamereq{} ->
      messages_pb:encode(#message{type = 'LEAVE_GAME_REQ', data = Encoded});
    #leavegameres{} ->
      messages_pb:encode(#message{type = 'LEAVE_GAME_RES', data = Encoded});
    #actionreq{} ->
      messages_pb:encode(#message{type = 'ACTION_REQ', data = Encoded});
    #actionres{} ->
      messages_pb:encode(#message{type = 'ACTION_RES', data = Encoded});
    #otherjointablentf{} ->
      messages_pb:encode(#message{type = 'OTHER_JOIN_TABLE_NTF', data = Encoded});
    #otherleavetablentf{} ->
      messages_pb:encode(#message{type = 'OTHER_LEAVE_TABLE_NTF', data = Encoded});
    #otheractionntf{} ->
      messages_pb:encode(#message{type = 'OTHER_ACTION_NTF', data = Encoded});
    _Other ->
      error
  end.
