(function() {
  var Player;

  module.exports = Player = (function() {
    function Player(game) {
      var container;
      this.game = game;
      container = game.add.group();
    }

    return Player;

  })();

}).call(this);
