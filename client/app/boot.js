(function() {
  var LoadingScreenGUI, LobbyScreenGUI, MenuScreenGUI, TableScreenGUI;

  LoadingScreenGUI = require('../gui/LoadingScreen');

  MenuScreenGUI = require('../gui/MenuScreen');

  LobbyScreenGUI = require('../gui/LobbyScreen');

  TableScreenGUI = require('../gui/TableScreen');

  module.exports = {
    create: function(game) {
      game.ws = (require('./ws'))(game);
      game.protocol = (require('./protocol'))(game);
      game.storage = (require('./storage'))(game);
      return EZGUI.Theme.load(['/public/assets/metalworks-theme/metalworks-theme.json'], function() {
        var LoadingScreen, LobbyScreen, MenuScreen, TableScreen;
        LoadingScreen = EZGUI.create(LoadingScreenGUI, 'metalworks');
        LoadingScreen.visible = false;
        MenuScreen = EZGUI.create(MenuScreenGUI, 'metalworks');
        MenuScreen.visible = false;
        LobbyScreen = EZGUI.create(LobbyScreenGUI, 'metalworks');
        LobbyScreen.visible = false;
        TableScreen = EZGUI.create(TableScreenGUI, 'metalworks');
        TableScreen.visible = false;
        return game.state.start('load');
      });
    }
  };

}).call(this);
