(function() {
  define(function(require) {
    return function(game) {
      var ws;
      ws = new WebSocket('ws://127.0.0.1:8080/ws');
      ws.binaryType = 'arraybuffer';
      ws.onopen = function() {
        return console.log('connected');
      };
      ws.onmessage = function(evt) {
        return console.log(game.protocol.onMessage(evt.data));
      };
      ws.onclose = function() {
        return console.log('disconnected');
      };
      ws.testProtocol = function() {
        var ActionReq, ActionType, JoinTableReq, LeaveGameReq, LeaveTableReq, ListTableReq, Message, MessageType, msg, req, _ref;
        _ref = game.protocol.Protocol, Message = _ref.Message, MessageType = _ref.MessageType, ActionType = _ref.ActionType, ListTableReq = _ref.ListTableReq, JoinTableReq = _ref.JoinTableReq, LeaveTableReq = _ref.LeaveTableReq, LeaveGameReq = _ref.LeaveGameReq, ActionReq = _ref.ActionReq;
        req = new ListTableReq();
        msg = new Message(MessageType.LIST_TABLE_REQ, req.toArrayBuffer());
        ws.send(msg.toArrayBuffer());
        req = new JoinTableReq();
        msg = new Message(MessageType.JOIN_TABLE_REQ, req.toArrayBuffer());
        ws.send(msg.toArrayBuffer());
        req = new LeaveTableReq();
        msg = new Message(MessageType.LEAVE_TABLE_REQ, req.toArrayBuffer());
        ws.send(msg.toArrayBuffer());
        req = new LeaveGameReq();
        msg = new Message(MessageType.LEAVE_GAME_REQ, req.toArrayBuffer());
        ws.send(msg.toArrayBuffer());
        req = new ActionReq(ActionType.ACTION_RAISE, 0);
        msg = new Message(MessageType.ACTION_REQ, req.toArrayBuffer());
        return ws.send(msg.toArrayBuffer());
      };
      return ws;
    };
  });

}).call(this);
