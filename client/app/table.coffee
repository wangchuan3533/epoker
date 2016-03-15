define (require) ->
  create: (game) ->
  
  update: (game) ->
    {table} = game.storage
    return if table.updated
    game.add.text game.world.centerX - 40, 100, 'table' + table.tableId.toString(), font: '35px Arial', fill: '#ff0044', align: 'center'
    
    for player, index in table.players
      game.add.text index * 200, 200, 'player' + player.id, font: '35px Arial', fill: '#ff0044', align: 'center'
    table.updated = true
    
    
