import React from 'react'
import Card from './Card'
import Container from './Container'

const CardList = ({x, y, cards}) => {
  return (
    <Container x={x} y={y}>
      {cards.map((card, index) => {
        return <Card x={index * 50} y={0} {...card} />
      })}
    </Container>
  )
}

export default CardList
