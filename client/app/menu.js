(function() {
  define(function(require) {
    return {
      create: function(game) {
        var start;
        start = game.add.text(game.world.centerX - 150, 400, 'Start Game', {
          font: '60px Arial',
          fill: '#ff0044',
          align: 'center'
        });
        start.inputEnabled = true;
        return start.events.onInputUp.add(function() {
          return game.state.start('game');
        });
      }
    };
  });

}).call(this);
