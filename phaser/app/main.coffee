game = new Phaser.Game 800, 600, Phaser.AUTO, ''
game.state.add 'boot', require './boot'
game.state.add 'load', require './load'
game.state.add 'menu', require './menu'
game.state.add 'lobby', require './lobby'
game.state.add 'table', require './table'
window.game = game

game.state.start 'boot'
