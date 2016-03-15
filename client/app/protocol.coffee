define (require) ->
  (game) ->
    protobuf = require 'protobuf'
    builder = protobuf.loadJsonFile '/proto/messages.json'
    
    Message = builder.build 'Message'
    TablePb = builder.build 'TablePb'
    PlayerPb = builder.build 'PlayerPb'
    MessageType = (builder.lookup 'Message.Type').object
    ActionType = (builder.lookup 'ActionReq.Action').object

    JoinTableReq = builder.build 'JoinTableReq'
    JoinTableRes = builder.build 'JoinTableRes'

    LeaveTableReq = builder.build 'LeaveTableReq'
    LeaveTableRes = builder.build 'LeaveTableRes'

    ListTableReq = builder.build 'ListTableReq'
    ListTableRes = builder.build 'ListTableRes'

    LeaveGameReq = builder.build 'LeaveGameReq'
    LeaveGameRes = builder.build 'LeaveGameRes'

    ActionReq = builder.build 'ActionReq'
    ActionRes = builder.build 'ActionRes'
    
    Protocol:
      Message: Message
      TablePb: TablePb
      PlayerPb: PlayerPb
      MessageType: MessageType
      ActionType: ActionType
      JoinTableReq: JoinTableReq
      JoinTableRes: JoinTableRes
      LeaveTableReq: LeaveGameReq
      LeaveTableRes: LeaveGameRes
      ListTableReq: ListTableReq
      ListTableRes: ListTableRes
      LeaveGameReq: LeaveGameReq
      ActionReq: ActionReq
      ActionRes: ActionRes
    
    onMessage: (data) ->
      msg = Message.decode data
      switch msg.type
        when MessageType.JOIN_TABLE_RES
          console.log 'JOIN_TABLE_RES'
          res = JoinTableRes.decode msg.data
          console.log res
          {table} = game.storage
          table.tableId = res.table.id
          table.players = res.table.players
          table.updated = false
        when MessageType.LEAVE_TABLE_RES
          console.log 'LEAVE_TABLE_RES'
          LeaveTableRes.decode msg.data
        when MessageType.LIST_TABLE_RES
          console.log 'LIST_TABLE_RES'
          res = ListTableRes.decode msg.data
          {lobby} = game.storage
          lobby.tables = res.table_ids
          lobby.updated = false
        when MessageType.LEAVE_GAME_RES
          console.log 'LEAVE_GAME_RES'
          LeaveGameRes.decode msg.data
        when MessageType.ACTION_RES
          console.log 'ACTION_RES'
          ActionRes.decode msg.data
        else null
