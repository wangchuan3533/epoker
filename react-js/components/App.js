import React from 'react'
import LoginDialog from '../containers/LoginDialog'
import Table from '../containers/Table'
const App = () => (
  <div style={{
    display: 'flex',
    flexDirection: 'row'
  }}>
    <LoginDialog />
    <Table />
  </div>
)

export default App
