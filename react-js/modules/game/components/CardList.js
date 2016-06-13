import React from 'react'
import Card from './Card'
import Group from './Group'

const CardList = (props) => {
  const {cards} = props
  return (
    <Group {...props}>
      {cards.map((card, index) => {
        return <Card x={index * 50} y={0} key={card} card={card} />
      })}
    </Group>
  )
}

export default CardList
