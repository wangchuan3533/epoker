LoadingScreenGUI = require '../gui/LoadingScreen'
MenuScreenGUI = require '../gui/MenuScreen'
LobbyScreenGUI = require '../gui/LobbyScreen'
TableScreenGUI = require '../gui/TableScreen'
module.exports =
  create: (game) ->
    game.ws = (require './ws') game
    game.protocol = (require './protocol') game
    game.storage = (require './storage') game
    
    # load ui
    EZGUI.Theme.load ['/public/assets/metalworks-theme/metalworks-theme.json'], ->
      LoadingScreen = EZGUI.create LoadingScreenGUI, 'metalworks'
      LoadingScreen.visible = false
      
      MenuScreen = EZGUI.create MenuScreenGUI, 'metalworks'
      MenuScreen.visible = false
      
      LobbyScreen = EZGUI.create LobbyScreenGUI, 'metalworks'
      LobbyScreen.visible = false
      
      TableScreen = EZGUI.create TableScreenGUI, 'metalworks'
      TableScreen.visible = false
      game.state.start 'load'
