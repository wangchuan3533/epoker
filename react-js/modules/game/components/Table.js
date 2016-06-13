import React from 'react'
import PlayerList from './PlayerList'
import CardList from './CardList'
const Table = ({tableId, players}) => (
  <svg width="800" height="600">
    <PlayerList x={0} y={0} players={players} />
    <CardList x={0} y={300} cards={[1, 20, 30, 34, 45]} />
  </svg>
)

export default Table
