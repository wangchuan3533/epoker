import React from 'react'
import LoginDialog from '../containers/LoginDialog'
import Player from './Player'
import Card from './Card'
const App = () => (
  <div style={{
    display: 'flex',
    flexDirection: 'row'
  }}>
    <LoginDialog />
    {[1, 10, 15, 20, 25, 30, 35.40, 45, 50].map((code) =>
      <Card key={code} card={code} />
    )}
    <Player status="folded" chips="1000" />
  </div>
)

export default App
