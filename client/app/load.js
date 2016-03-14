(function() {
  define(function(require) {
    return {
      preload: function(game) {},
      create: function(game) {
        return game.state.start('menu');
      }
    };
  });

}).call(this);
