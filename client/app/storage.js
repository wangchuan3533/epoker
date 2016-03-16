(function() {
  module.exports = function(game) {
    return {
      lobby: {
        updated: true,
        tables: []
      },
      table: {
        updated: true,
        tableId: -1,
        players: []
      }
    };
  };

}).call(this);
