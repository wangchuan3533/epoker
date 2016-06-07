import React from 'react'
import Container from './Container'
import CardList from './CardList'

const Player = ({x, y, name, chips}) => (
  <Container x={x} y={y}>
    <rect width="180" height="100" style={{
        fill: 'blue',
        fillOpacity: 0.1,
        stokeOpacity: 0.9
      }}/>
    <text x="30" y="40" fill="red">
      {name}
    </text>
    <text x="30" y="60" fill="black">
      {chips}
    </text>
    <CardList x="90" y="20" cards={[{key: 20, card: 20}, {key: 2, card: 2}]} />
  </Container>
)

export default Player
