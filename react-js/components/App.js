import React from 'react'
import LoginDialog from '../containers/LoginDialog'
import Player from './Player'
import Card from './Card'
const App = () => (
  <div style={{float: 'left'}}>
    <LoginDialog />
    <Card card={1} />
    <Card card={10} />
    <Card card={15} />
    <Card card={20} />
    <Card card={25} />
    <Card card={30} />
    <Card card={35} />
    <Card card={40} />
    <Card card={45} />
    <Card card={50} />
  </div>
)

export default App
