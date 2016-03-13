define (require) ->
  _Phaser = require 'phaser'
  _ = require 'underscore'

  game = null
  star = null
  ws = null

  preload = ->
    game.load.image 'star', 'assets/star.png'

  create = ->
    star = game.add.sprite 400, 300, 'star'

  game = new Phaser.Game 800, 600, Phaser.AUTO, '', {preload: preload, create: create}

  ws = new WebSocket 'ws://127.0.0.1:8080/echo'
  ws.onopen = ->
    console.log 'websocket connected'
    ws.send JSON.stringify type: 'list'

  ws.onmessage = (evt) ->
    console.log 'Received message ' + evt.data

  ws.onclose = ->
    console.log 'websocket closed'

  window.testSend = (data) ->
    ws.send JSON.stringify data
