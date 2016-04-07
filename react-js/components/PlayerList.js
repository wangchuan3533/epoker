import React from 'react'
import Player from './Player'

const PlayerList = ({players}) => {
  return (
    <div>
      {players.map((player) => {
        return <Player key={player.id} {...player} />
      })}
    </div>
  )
}

export default PlayerList
