import React from 'react'
import styles from 'material-ui/lib/styles'

const colors = styles.Colors

const Card = ({card}) => {
  let suit, color, rank, style
  
  switch (Math.floor(card / 13)) {
    case 0:
      suit = '♥'
      color = colors.red500
      break
    case 1:
      suit = '♠'
      color = colors.black
      break
    case 2:
      suit = '♦'
      color = colors.red500
      break
    case 3:
      suit = '♣'
      color = colors.black
      break
  }
  rank = card % 13
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
  
  style = {
    width: 50,
    height: 70,
    position: 'relative',
    borderStyle: 'solid',
    borderWidth: 1,
    margin: 5,
    fontSize: 16,
    textAlign: 'center',
    color
  }
  return (
    <div style={style}>
      <div style={{
        position: 'absolute',
        top: 0,
        left: 0
      }}>
        {suit}
      </div>
      <div style={{
        position: 'absolute',
        top: '36%',
        left: '37%',
      }}>
        {rank}
      </div>
      <div style={{
        position: 'absolute',
        right: 0,
        bottom: 0
      }}>
        {suit}
      </div>
    </div>
  )
}

export default Card
