(function() {
  module.exports = {
    create: function(game) {
      return EZGUI.components.TableScreen.visible = true;
    },
    update: function(game) {
      var index, player, table, _i, _len, _ref;
      table = game.storage.table;
      if (table.updated) {
        return;
      }
      game.add.text(game.world.centerX - 40, 100, 'table' + table.tableId.toString(), {
        font: '35px Arial',
        fill: '#ff0044',
        align: 'center'
      });
      _ref = table.players;
      for (index = _i = 0, _len = _ref.length; _i < _len; index = ++_i) {
        player = _ref[index];
        game.add.text(index * 200, 200, 'player' + player.id, {
          font: '35px Arial',
          fill: '#ff0044',
          align: 'center'
        });
      }
      return table.updated = true;
    }
  };

}).call(this);
