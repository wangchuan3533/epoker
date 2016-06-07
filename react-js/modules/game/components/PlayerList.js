import React from 'react'
import Container from './Container'
import Player from './Player'

const PlayerList = ({x, y, players}) => (
  <Container x={x} y={y} >
    {players.map((player, index) => (<Player x={200 * index} y={300} key={player.id} {...player} />))}
  </Container>
)

export default PlayerList
