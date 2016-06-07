import React from 'react'
import {
  black,
  red500
} from 'material-ui/styles/colors'
import Container from './Container'

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


const Card = ({x, y, card}) => {
  const {suit, color, rank} = getStyle(card)
  const textStyle = { fontSize: 10, fill: color }
  
  return (
    <Container x={x} y={y}>
      <rect height="56" width="40" style={containerStyle} />
      <text x="3" y="10" style={textStyle}>
        {suit}
      </text>
      <text x="16" y="31" style={textStyle}>
        {rank}
      </text>
      <text x="32" y="53" style={textStyle}>
        {suit}
      </text>
    </Container>
  )
}

export default Card
