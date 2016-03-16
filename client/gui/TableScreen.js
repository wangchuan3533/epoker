(function() {
  module.exports = {
    id: 'TableScreen',
    component: 'Window',
    padding: 4,
    position: {
      x: 0,
      y: 0
    },
    width: 800,
    height: 600,
    layout: [1, 1],
    children: [
      {
        id: 'TableScreenList',
        component: 'List',
        position: 'center',
        width: 700,
        height: 500,
        layout: [1, 5],
        children: [
          {
            id: 'table1',
            component: 'Button',
            text: 'Player1',
            position: 'center',
            userData: 'table1',
            width: 500,
            height: 65
          }
        ]
      }
    ]
  };

}).call(this);
