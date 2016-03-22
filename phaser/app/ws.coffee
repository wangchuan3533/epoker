module.exports =
  (game) ->
    ws = new WebSocket 'ws://127.0.0.1:8080/ws'
    ws.binaryType = 'arraybuffer'
    
    ws.onopen = ->
      console.log 'connected'
      @testProtocol()
    ws.onmessage = (evt)->
      console.log game.protocol.onMessage evt.data
    ws.onclose = ->
      console.log 'disconnected'
    ws.testProtocol = ->
      {Message, MessageType, ActionType, ListTableReq, JoinTableReq, LeaveTableReq, LeaveGameReq, ActionReq} = game.protocol.Protocol
      
      # list table
      req = new ListTableReq()
      msg = new Message MessageType.LIST_TABLE_REQ, req.toArrayBuffer()
      ws.send msg.toArrayBuffer()

      # join table
      req = new JoinTableReq()
      msg = new Message MessageType.JOIN_TABLE_REQ, req.toArrayBuffer()
      ws.send msg.toArrayBuffer()

      # leave table
      req = new LeaveTableReq()
      msg = new Message MessageType.LEAVE_TABLE_REQ, req.toArrayBuffer()
      ws.send msg.toArrayBuffer()

      # leave game
      req = new LeaveGameReq()
      msg = new Message MessageType.LEAVE_GAME_REQ, req.toArrayBuffer()
      ws.send msg.toArrayBuffer()

      # action
      req = new ActionReq ActionType.ACTION_RAISE, 0
      msg = new Message MessageType.ACTION_REQ, req.toArrayBuffer()
      ws.send msg.toArrayBuffer()

    ws
