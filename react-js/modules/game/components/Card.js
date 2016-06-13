import React from 'react'
import {
  black,
  red500
} from 'material-ui/styles/colors'
import Group from './Group'

const getStyle = (card) => {
  let suit, color, rank
  
  switch (Math.floor(card / 13)) {
    case 0:
      suit = '♥'
      color = red500
      break
    case 1:
      suit = '♠'
      color = black
      break
    case 2:
      suit = '♦'
      color = red500
      break
    case 3:
      suit = '♣'
      color = black
      break
  }
  
  rank = Math.floor(card % 13)
  if (rank < 9) {
    rank = '' + (rank + 2)
  } else if (rank == 9) {
    rank = 'J'
  } else if (rank == 10) {
    rank = 'Q'
  } else if (rank == 11) {
    rank = 'K'
  } else if (rank == 12) {
    rank = 'A'
  }
  
  return {suit, color, rank}
}

const containerStyle = {
  fillOpacity: 0.1,
}


const Card = (props) => {
  const {card} = props
  const {suit, color, rank} = getStyle(card)
  const suitStyle = { fontSize: 12, fill: color }
  const rankStyle = { fontSize: 16, fill: color }
  
  return (
    <Group {...props}>
      <rect height="56" width="40" style={containerStyle} />
      <text x="6" y="10" style={suitStyle}>
        {rank}
      </text>
      <text x="15" y="34" style={rankStyle}>
        {suit}
      </text>
      <text x="29" y="53" style={suitStyle}>
        {rank}
      </text>
    </Group>
  )
}

export default Card
