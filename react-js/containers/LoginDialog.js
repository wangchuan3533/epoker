import { connect } from 'react-redux'
import jquery from 'jquery'
import {loginSuccess} from '../actions'
import LoginForm from '../components/LoginForm'

const mapStateToProps = (state) => {
  return {
    open: !state.login.authorized,
  }
}

const mapDispatchToProps = (dispatch) => {
  return {
    onLoginClick: (username, password) => {
      jquery.ajax({
        type: 'POST',
        url: '/login',
        contentType: 'application/json; charset=utf-8',
        dataType: 'json',
        data: JSON.stringify({
          uid: username,
          password: password,
        }),
        success: (data) => {
          dispatch(loginSuccess())
        },
        failure: (err) => {
          console.log(err)
        }
      })
    },
    onRegisterClick: (username, password) => {
      jquery.ajax({
        type: 'POST',
        url: '/login',
        contentType: 'application/json; charset=utf-8',
        dataType: 'json',
        data: JSON.stringify({
          register: true,
          uid: username,
          password: password,
        }),
        success: (data) => {
          dispatch(loginSuccess())
        },
        failure: (err) => {
          console.log(err)
        }
      })
    }
  }
}
const LoginDialog = connect(mapStateToProps, mapDispatchToProps)(LoginForm)
export default LoginDialog
