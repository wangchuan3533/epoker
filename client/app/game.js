(function() {
  define(function(require) {
    return {
      create: function(game) {
        var JoinTableReq, Message, builder, debug, msg, protobuf, req, ws;
        protobuf = require('protobuf');
        builder = protobuf.loadJsonFile('/proto/messages.json');
        Message = builder.build('Message');
        JoinTableReq = builder.build('JoinTableReq');
        req = new JoinTableReq();
        console.log(req.table_id);
        msg = new Message({
          type: "JOIN_TABLE_REQ",
          data: req.toArrayBuffer()
        });
        ws = game.ws;
        debug = game.add.text(game.world.centerX - 120, 400, 'debug', {
          font: '35px Arial',
          fill: '#ff0044',
          align: 'center'
        });
        return ws.send(msg.toArrayBuffer());
      }
    };
  });

}).call(this);
