module.exports =
  preload: (game) ->

  create: (game) ->
    {ws, protocol} = game
    {Message, MessageType, JoinTableReq} = protocol.Protocol
    
    EZGUI.components.LobbyScreen.visible = true
    
    EZGUI.components.JoinTableBtn.on 'click', ->
      console.log 'join table clicked'
      req = new JoinTableReq -1
      msg = new Message MessageType.JOIN_TABLE_REQ, req.toArrayBuffer()
      ws.send msg.toArrayBuffer()
      EZGUI.components.LobbyScreen.visible = false
      game.state.start 'table'
    
  update: (game) ->
