define (require) ->
  create: (game) ->
    game.ws = (require './ws') game
    game.protocol = (require './protocol') game
    game.storage = (require './storage') game
    game.state.start 'load'
    window.game = game
