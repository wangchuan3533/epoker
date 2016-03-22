module.exports =
  preload: (game) ->

  create: (game) ->
    EZGUI.components.LoadingScreen.visible = true
    
    setTimeout (->
      EZGUI.components.LoadingScreen.visible = false
      game.state.start 'menu'
    ), 1000
