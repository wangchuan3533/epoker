(function() {
  define(function(require) {
    return {
      preload: function(game) {},
      create: function(game) {
        var JoinTableReq, ListTableReq, Message, MessageType, index, lobby, msg, protocol, req, storage, table, ws, _i, _len, _ref, _ref1;
        ws = game.ws, protocol = game.protocol, storage = game.storage;
        _ref = protocol.Protocol, Message = _ref.Message, MessageType = _ref.MessageType, ListTableReq = _ref.ListTableReq, JoinTableReq = _ref.JoinTableReq;
        lobby = storage.lobby;
        _ref1 = lobby.tables;
        for (index = _i = 0, _len = _ref1.length; _i < _len; index = ++_i) {
          table = _ref1[index];
          game.add.text(index * 10, index * 10, table);
        }
        req = new ListTableReq();
        msg = new Message(MessageType.LIST_TABLE_REQ, req.toArrayBuffer());
        return ws.send(msg.toArrayBuffer());
      },
      update: function(game) {
        var JoinTableReq, Message, MessageType, lobby, protocol, storage, table, tableId, text, ws, _i, _len, _ref, _ref1;
        ws = game.ws, protocol = game.protocol, storage = game.storage;
        _ref = protocol.Protocol, Message = _ref.Message, MessageType = _ref.MessageType, JoinTableReq = _ref.JoinTableReq;
        lobby = storage.lobby, table = storage.table;
        if (lobby.updated) {
          return;
        }
        _ref1 = lobby.tables;
        for (_i = 0, _len = _ref1.length; _i < _len; _i++) {
          tableId = _ref1[_i];
          text = game.add.text(game.world.centerX - 40, tableId * 100, tableId.toString(), {
            font: '35px Arial',
            fill: '#ff0044',
            align: 'center'
          });
          text.inputEnabled = true;
          text.events.onInputUp.add(function() {
            var msg, req;
            req = new JoinTableReq(tableId);
            msg = new Message(MessageType.JOIN_TABLE_REQ, req.toArrayBuffer());
            ws.send(msg.toArrayBuffer());
            return game.state.start('table');
          });
        }
        return lobby.updated = true;
      }
    };
  });

}).call(this);
