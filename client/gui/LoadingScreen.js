(function() {
  module.exports = {
    id: 'LoadingScreen',
    component: 'Window',
    padding: 4,
    position: {
      x: 0,
      y: 0
    },
    width: 800,
    height: 600,
    layout: [1, 2],
    children: [
      null, {
        id: 'LoadingScreenTitle',
        component: 'Label',
        text: 'Loading Game ...',
        position: 'center',
        font: {
          size: '50px',
          family: 'Skranji',
          color: '#8f8'
        },
        width: 800,
        height: 100
      }
    ]
  };

}).call(this);
