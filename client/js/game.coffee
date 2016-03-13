game = null
star = null

preload = ->
  game.load.image 'sky', 'assets/sky.png'
  game.load.image 'star', 'assets/star.png'

create = ->
  game.add.sprite 0, 0, 'sky'
  star = game.add.sprite 400, 300, 'star'

update = ->
  star.x = 800 * Math.random()
  star.y = 600 * Math.random()

game = new Phaser.Game 800, 600, Phaser.AUTO, '', {preload: preload, create: create, update: update}


ws = new WebSocket 'ws://127.0.0.1:8080/echo'
ws.onopen = ->
  console.log 'websocket connected'

ws.onmessage = (evt) ->
  console.log 'Received message ' + evt.data
  
ws.onclose = ->
  console.log 'websocket closed'

window.test = (data) ->
  ws.send JSON.stringify data
