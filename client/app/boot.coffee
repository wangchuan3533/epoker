define (require) ->
  create: (game) ->
    game.ws = require './ws'
    game.state.start 'load'
    
