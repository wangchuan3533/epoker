define (require) ->
  _Phaser = require 'phaser'
  _ = require 'underscore'
  
  game = new Phaser.Game 800, 600, Phaser.AUTO, ''
  game.state.add 'boot', require './boot'
  game.state.add 'load', require './load'
  game.state.add 'menu', require './menu'
  game.state.add 'game', require './game'
  
  game.state.start 'boot'
