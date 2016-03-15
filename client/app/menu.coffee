define (require) ->
  create: (game) ->
    start =  game.add.text game.world.centerX - 150, 400, 'Start Game', font: '60px Arial', fill: '#ff0044', align: 'center'
    start.inputEnabled = true
    start.events.onInputUp.add ->
      game.state.start 'lobby'
