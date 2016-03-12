game = null
star = null

preload = ->
  game.load.image 'sky', 'assets/sky.png'
  game.load.image 'star', 'assets/star.png'

create = ->
  game.add.sprite 0, 0, 'sky'
  star = game.add.sprite 100, 100, 'star'

update = ->

game = new Phaser.Game 800, 600, Phaser.AUTO, '', {preload: preload, create: create, update: update}
