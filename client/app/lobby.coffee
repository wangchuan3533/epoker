define (require) ->
  preload: (game) ->

  create: (game) ->
    {ws, protocol, storage} = game
    {Message, MessageType, ListTableReq, JoinTableReq} = protocol.Protocol
    {lobby} = storage
    
    # render tables
    for table, index in lobby.tables
      game.add.text index * 10, index * 10, table
    
    req = new ListTableReq()
    msg = new Message MessageType.LIST_TABLE_REQ, req.toArrayBuffer()
    ws.send msg.toArrayBuffer()
    
  update: (game) ->
    {ws, protocol, storage} = game
    {Message, MessageType, JoinTableReq} = protocol.Protocol
    {lobby, table} = storage
    return if lobby.updated
    
    for tableId in lobby.tables
      text = game.add.text game.world.centerX - 40, tableId * 100, tableId.toString(), font: '35px Arial', fill: '#ff0044', align: 'center'
      text.inputEnabled = true
      text.events.onInputUp.add ->
        req = new JoinTableReq tableId
        msg = new Message MessageType.JOIN_TABLE_REQ, req.toArrayBuffer()
        ws.send msg.toArrayBuffer()
        game.state.start 'table'
    lobby.updated = true
      
