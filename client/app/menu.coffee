module.exports =
  create: (game) ->
    EZGUI.components.MenuScreen.visible = true
    EZGUI.components.MenuScreenStartBtn.on 'click', ->
      EZGUI.components.MenuScreen.visible = false
      game.state.start 'lobby'
