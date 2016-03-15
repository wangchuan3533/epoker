(function() {
  define(function(require) {
    return {
      create: function(game) {
        game.ws = (require('./ws'))(game);
        game.protocol = (require('./protocol'))(game);
        game.storage = (require('./storage'))(game);
        game.state.start('load');
        return window.game = game;
      }
    };
  });

}).call(this);
