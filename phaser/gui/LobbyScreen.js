(function() {
  module.exports = {
    id: 'LobbyScreen',
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
        id: 'LobbyScreenList',
        component: 'List',
        position: 'center',
        width: 700,
        height: 500,
        dragY: true,
        dragX: false,
        layout: [1, 3],
        children: [
          {
            id: 'JoinTableBtn',
            component: 'Button',
            text: 'Join Table',
            position: 'center',
            width: 500,
            height: 65
          }
        ]
      }
    ]
  };

}).call(this);
