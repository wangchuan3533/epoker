(function() {
  define(function(require) {
    return {
      create: function(game) {
        game.ws = require('./ws');
        return game.state.start('load');
      }
    };
  });

}).call(this);
