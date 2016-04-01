import { connect } from 'react-redux'
import fetch from 'isomorphic-fetch'
import {login} from '../actions'
import LoginForm from '../components/LoginForm'

const mapStateToProps = (state) => {
  return {
    open: !state.login.authorized,
  }
}

const mapDispatchToProps = (dispatch) => {
  return {
    onLoginClick: (username, password) => {dispatch(login(username, password))},
    onRegisterClick: (username, password) => {}
  }
}
const LoginDialog = connect(mapStateToProps, mapDispatchToProps)(LoginForm)
export default LoginDialog
