import React from 'react'
import Immutable from 'immutable'
import {connect} from 'react-redux'
import { bindActionCreators } from 'redux'
import Table from './components/Table'

import * as GameActions from './actions'

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
      <div>
        <Table tableId={state.get('tableId')} players={state.get('players')} />
      </div>
    )
  }
}

export default connect(state => ({state: state.game}))(Main)
