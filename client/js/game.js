var create, game, preload, star, update;

game = null;

star = null;

preload = function() {
  game.load.image('sky', 'assets/sky.png');
  return game.load.image('star', 'assets/star.png');
};

create = function() {
  game.add.sprite(0, 0, 'sky');
  return star = game.add.sprite(100, 100, 'star');
};

update = function() {};

game = new Phaser.Game(800, 600, Phaser.AUTO, '', {
  preload: preload,
  create: create,
  update: update
});
