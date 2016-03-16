module.exports =
  id: 'MenuScreen'
  component: 'Window'
  padding: 4
  position:
    x: 0
    y: 0
  width: 800
  height: 600
  layout: [
    1
    4
  ]
  children: [
    {
      id: 'MainScreenTitle'
      component: 'Label'
      text: 'Texas Hold\'em Poker'
      position: 'center'
      font:
        size: '50px'
        family: 'Skranji'
        color: '#8f8'
      width: 800
      height: 100
    }
    null
    {
      id: 'MenuScreenStartBtn'
      component: 'Button'
      text: 'Start'
      position: 'center'
      width: 250
      height: 65
    }
    {
      id: 'MenuScreenOptionsBtn'
      component: 'Button'
      text: 'Options'
      position: 'center'
      width: 250
      height: 65
    }
  ]
