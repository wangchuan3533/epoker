import fetch from 'isomorphic-fetch'
import transport from '../../transport'
import {changeRoute} from '../app/actions'
import {wsConnected} from '../game/actions'

export const SHOW_LOGIN_DIALOG = 'SHOW_LOGIN_DIALOG'
export const showLoginDialog = () => ({ type: SHOW_LOGIN_DIALOG })
export const HIDE_LOGIN_DIALOG = 'HIDE_LOGIN_DIALOG'
export const hideLoginDialog = () => ({ type: HIDE_LOGIN_DIALOG })

export const LOGIN_STARTED = 'LOGIN_STARTED'
export const loginStarted = () => ({type: LOGIN_STARTED})
export const LOGIN_FAILED = 'LOGIN_FAILED'
export const loginFailed = () => ({type: LOGIN_FAILED})
export const LOGIN_SUCESS = 'LOGIN_SUCESS'
export const loginSuccess = () => ({type: LOGIN_SUCESS})

export const login = ({name, password}) => (dispatch) => {
  dispatch(loginStarted())
  return fetch('/login', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json; charset=utf-8',
      'Accept': 'application/json'
    },
    body: JSON.stringify({
      uid: name,
      password: password,
    })
  })
  .then(response => response.json())
  .then(data => {
    dispatch(loginSuccess())
    return data.token
  })
  .then(token => {
    transport.connect('ws://127.0.0.1:8080/ws/' + token)
    .then(() => {
      dispatch(changeRoute('/login/game'))
      dispatch(wsConnected(transport))
    })
  })
}
