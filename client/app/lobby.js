(function() {
  module.exports = {
    preload: function(game) {},
    create: function(game) {
      var JoinTableReq, Message, MessageType, protocol, ws, _ref;
      ws = game.ws, protocol = game.protocol;
      _ref = protocol.Protocol, Message = _ref.Message, MessageType = _ref.MessageType, JoinTableReq = _ref.JoinTableReq;
      EZGUI.components.LobbyScreen.visible = true;
      return EZGUI.components.JoinTableBtn.on('click', function() {
        var msg, req;
        console.log('join table clicked');
        req = new JoinTableReq(-1);
        msg = new Message(MessageType.JOIN_TABLE_REQ, req.toArrayBuffer());
        ws.send(msg.toArrayBuffer());
        EZGUI.components.LobbyScreen.visible = false;
        return game.state.start('table');
      });
    },
    update: function(game) {}
  };

}).call(this);
