define(function(require) {
  var _, _Phaser, create, game, preload, star, ws;
  _Phaser = require('phaser');
  _ = require('underscore');
  game = null;
  star = null;
  ws = null;
  preload = function() {
    return game.load.image('star', 'assets/star.png');
  };
  create = function() {
    return star = game.add.sprite(400, 300, 'star');
  };
  game = new Phaser.Game(800, 600, Phaser.AUTO, '', {
    preload: preload,
    create: create
  });
  ws = new WebSocket('ws://127.0.0.1:8080/echo');
  ws.onopen = function() {
    console.log('websocket connected');
    return ws.send(JSON.stringify({
      type: 'list'
    }));
  };
  ws.onmessage = function(evt) {
    return console.log('Received message ' + evt.data);
  };
  ws.onclose = function() {
    return console.log('websocket closed');
  };
  return window.testSend = function(data) {
    return ws.send(JSON.stringify(data));
  };
});
