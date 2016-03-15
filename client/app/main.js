(function() {
  define(function(require) {
    var game, _, _Phaser;
    _Phaser = require('phaser');
    _ = require('underscore');
    game = new Phaser.Game(800, 600, Phaser.AUTO, '');
    game.state.add('boot', require('./boot'));
    game.state.add('load', require('./load'));
    game.state.add('menu', require('./menu'));
    game.state.add('lobby', require('./lobby'));
    game.state.add('table', require('./table'));
    return game.state.start('boot');
  });

}).call(this);
