(function() {
  module.exports = {
    preload: function(game) {},
    create: function(game) {
      EZGUI.components.LoadingScreen.visible = true;
      return setTimeout((function() {
        EZGUI.components.LoadingScreen.visible = false;
        return game.state.start('menu');
      }), 1000);
    }
  };

}).call(this);
