import React from 'react'
import Player from './Player'

const PlayerList = ({players}) => (
  <div>
    {players.map(player => (<Player key={player.id} name="hello" chips="100" />))}
  </div>
)

export default PlayerList
