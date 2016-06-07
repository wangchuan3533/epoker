import React from 'react'
import Immutable from 'immutable'
import {connect} from 'react-redux'
import { bindActionCreators } from 'redux'
import LoginDialog from './components/LoginDialog'
import * as loginActions from './actions'

class Main extends React.Component {
  constructor(props) {
    super(props)
  }
  componentDidMount() {
    const {dispatch} = this.props
  }
  
  render() {
    const { state, dispatch, form, children} = this.props
    const actions = bindActionCreators(loginActions, dispatch)
    
    return (
      <div>
        <LoginDialog state={state} actions={actions} form={form} />
        <div>
          {children}
        </div>
      </div>
    )
  }
}

export default connect(state => ({state: state.login, form: state.form.loginForm}))(Main)
