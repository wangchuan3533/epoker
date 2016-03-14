define (require) ->
  ws = new WebSocket 'ws://127.0.0.1:8080/echo'
  ws.binaryType = 'arraybuffer'
  
  ws.onopen = ->
    console.log 'connected'
  ws.onmessage = (evt)->
    console.log evt.data
  ws.onclose = ->
    console.log 'disconnected'
  
  ws
