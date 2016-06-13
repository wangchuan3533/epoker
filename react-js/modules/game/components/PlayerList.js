import React from 'react'
import Group from './Group'
import Player from './Player'

const PlayerList = (props) => {
  const {players} = props
  
  return (
    <Group {...props} >
      {players.map((player, index) => (<Player x={200 * index} y={100} key={player.id} {...player} />))}
    </Group>
  )
}

export default PlayerList
