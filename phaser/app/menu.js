(function() {
  module.exports = {
    create: function(game) {
      EZGUI.components.MenuScreen.visible = true;
      return EZGUI.components.MenuScreenStartBtn.on('click', function() {
        EZGUI.components.MenuScreen.visible = false;
        return game.state.start('lobby');
      });
    }
  };

}).call(this);
