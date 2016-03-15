(function() {
  define(function(require) {
    var Player;
    return Player = (function() {
      function Player(game) {
        var container;
        this.game = game;
        container = game.add.group();
      }

      return Player;

    })();
  });

}).call(this);
