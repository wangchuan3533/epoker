import React from 'react'
import Card from './Card'

const CardList = ({cards}) => {
  return (
    <div style={{
        display: 'flex',
        flexDirection: 'row'
      }}>
      {cards.map((card) => {
        return <Card {...card} />
      })}
    </div>
  )
}

export default CardList
