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
    width: 40,
    height: 56,
    position: 'relative',
    borderStyle: 'dotted',
    borderWidth: 1,
    margin: 4,
    color
  }
  return (
    <div style={style}>
      <div style={{
        width: '100%',
        position: 'absolute',
        top: 0,
        textAlign: 'left'
      }}>
        {suit}
      </div>
      <div style={{
        width: '100%',
        position: 'absolute',
        top: '37%',
        textAlign: 'center'
      }}>
        {rank}
      </div>
      <div style={{
        width: '100%',
        position: 'absolute',
        bottom: 0,
        textAlign: 'right'
      }}>
        {suit}
      </div>
    </div>
  )
}

export default Card
