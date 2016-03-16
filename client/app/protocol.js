(function() {
  module.exports = function(game) {
    var ActionReq, ActionRes, ActionType, JoinTableReq, JoinTableRes, LeaveGameReq, LeaveGameRes, LeaveTableReq, LeaveTableRes, ListTableReq, ListTableRes, Message, MessageType, PlayerPb, TablePb, builder, protobuf;
    protobuf = require('protobufjs');
    builder = protobuf.loadJsonFile('/proto/messages.json');
    Message = builder.build('Message');
    TablePb = builder.build('TablePb');
    PlayerPb = builder.build('PlayerPb');
    MessageType = (builder.lookup('Message.Type')).object;
    ActionType = (builder.lookup('ActionReq.Action')).object;
    JoinTableReq = builder.build('JoinTableReq');
    JoinTableRes = builder.build('JoinTableRes');
    LeaveTableReq = builder.build('LeaveTableReq');
    LeaveTableRes = builder.build('LeaveTableRes');
    ListTableReq = builder.build('ListTableReq');
    ListTableRes = builder.build('ListTableRes');
    LeaveGameReq = builder.build('LeaveGameReq');
    LeaveGameRes = builder.build('LeaveGameRes');
    ActionReq = builder.build('ActionReq');
    ActionRes = builder.build('ActionRes');
    return {
      Protocol: {
        Message: Message,
        TablePb: TablePb,
        PlayerPb: PlayerPb,
        MessageType: MessageType,
        ActionType: ActionType,
        JoinTableReq: JoinTableReq,
        JoinTableRes: JoinTableRes,
        LeaveTableReq: LeaveGameReq,
        LeaveTableRes: LeaveGameRes,
        ListTableReq: ListTableReq,
        ListTableRes: ListTableRes,
        LeaveGameReq: LeaveGameReq,
        ActionReq: ActionReq,
        ActionRes: ActionRes
      },
      onMessage: function(data) {
        var lobby, msg, res, table;
        msg = Message.decode(data);
        switch (msg.type) {
          case MessageType.JOIN_TABLE_RES:
            console.log('JOIN_TABLE_RES');
            res = JoinTableRes.decode(msg.data);
            console.log(res);
            table = game.storage.table;
            table.tableId = res.table.id;
            table.players = res.table.players;
            return table.updated = false;
          case MessageType.LEAVE_TABLE_RES:
            console.log('LEAVE_TABLE_RES');
            return LeaveTableRes.decode(msg.data);
          case MessageType.LIST_TABLE_RES:
            console.log('LIST_TABLE_RES');
            res = ListTableRes.decode(msg.data);
            lobby = game.storage.lobby;
            lobby.tables = res.table_ids;
            return lobby.updated = false;
          case MessageType.LEAVE_GAME_RES:
            console.log('LEAVE_GAME_RES');
            return LeaveGameRes.decode(msg.data);
          case MessageType.ACTION_RES:
            console.log('ACTION_RES');
            return ActionRes.decode(msg.data);
          default:
            return null;
        }
      }
    };
  };

}).call(this);
