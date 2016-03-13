(function() {
  var create, game, preload, star, update, ws;

  game = null;

  star = null;

  preload = function() {
    game.load.image('sky', 'assets/sky.png');
    return game.load.image('star', 'assets/star.png');
  };

  create = function() {
    game.add.sprite(0, 0, 'sky');
    return star = game.add.sprite(400, 300, 'star');
  };

  update = function() {
    star.x = 800 * Math.random();
    return star.y = 600 * Math.random();
  };

  game = new Phaser.Game(800, 600, Phaser.AUTO, '', {
    preload: preload,
    create: create,
    update: update
  });

  ws = new WebSocket('ws://127.0.0.1:8080/echo');

  ws.onopen = function() {
    return console.log('websocket connected');
  };

  ws.onmessage = function(evt) {
    return console.log('Received message ' + evt.data);
  };

  ws.onclose = function() {
    return console.log('websocket closed');
  };

  window.test = function(data) {
    return ws.send(JSON.stringify(data));
  };

}).call(this);
