(function() {
  define(function(require) {
    return function(game) {
      return {
        lobby: {
          updated: true,
          tables: []
        },
        table: {
          updated: true,
          tableId: -1
        }
      };
    };
  });

}).call(this);
