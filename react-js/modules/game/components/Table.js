import React from 'react'
import PlayerList from './PlayerList'

const Table = ({tableId, players}) => (
  <svg width="1100" height="1000">
    <PlayerList x={0} y={0} players={players} />
  </svg>
)

export default Table
