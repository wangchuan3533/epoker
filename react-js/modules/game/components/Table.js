import React from 'react'
import PlayerList from './PlayerList'

const Table = ({tableId, players}) => (
  <div>
    <PlayerList players={players} />
  </div>
)

export default Table
