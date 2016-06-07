import React from 'react'
import Immutable from 'immutable'
import {connect} from 'react-redux'
import { bindActionCreators } from 'redux'
import * as AppActions from './actions'

class Main extends React.Component {
  constructor(props) {
    super(props)
  }
  componentDidMount() {
    const {dispatch} = this.props
  }
  
  render() {
    const { state, dispatch, children} = this.props
    const actions = bindActionCreators(AppActions, dispatch)
    
    return (
      <div>
        {children}
      </div>
    )
  }
}

export default connect(state => ({state: state.app}))(Main)
