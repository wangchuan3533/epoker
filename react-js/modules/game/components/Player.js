import React from 'react'
import Group from './Group'
import CardList from './CardList'

const Player = (props) => {
  const {name, chips} = props
  return (
    <Group {...props}>
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
      <CardList x={90} y={20} cards={[20, 30]} />
    </Group>
  )
}

export default Player
