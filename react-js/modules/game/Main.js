import React from 'react'
import Immutable from 'immutable'
import {connect} from 'react-redux'
import { bindActionCreators } from 'redux'
import Table from './components/Table'
import Controls from './components/Controls'

import * as GameActions from './actions'
const containerStyle = {
}
const tableStyle = {
  margin: 'auto',
  width: '80%',
}
const controlStyle = {
  margin: 'auto',
  width: '80%',
}

class Main extends React.Component {
  constructor(props) {
    super(props)
  }
  componentDidMount() {
    const {dispatch} = this.props
  }
  
  render() {
    const { state, dispatch} = this.props
    const actions = bindActionCreators(GameActions, dispatch)
    
    return (
      <div style={containerStyle}>
        <div style={tableStyle}>
          <Table tableId={state.get('tableId')} players={state.get('players')} />
        </div>
        {state.get('started') ? (
          <div style={controlStyle}>
            <Controls />
          </div>
        ) : null}
      </div>
    )
  }
}

export default connect(state => ({state: state.game}))(Main)
