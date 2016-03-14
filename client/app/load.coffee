define (require) ->
  preload: (game) ->
  
  create: (game) ->
    game.state.start 'menu'
    
