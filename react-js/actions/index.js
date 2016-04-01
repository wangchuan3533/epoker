import fetch from 'isomorphic-fetch'
import ws from '../protocols/ws'

export const LOGIN_STARTED = 'LOGIN_STARTED'
const loginStarted = () => {
  return {
    type: LOGIN_STARTED
  }
}

export const LOGIN_SUCCESS = 'LOGIN_SUCCESS'
const loginSuccess = () => {
  return {
    type: LOGIN_SUCCESS
  }
}

export const LOGIN_FAILED = 'LOGIN_FAILED'
const loginFailed = (errmsg) => {
  return {
    type: LOGIN_FAILED,
    errmsg
  }
}


export const WS_CONNECTED = 'WS_CONNECTED'
const wsConnected = () => {
  return {
    type: WS_CONNECTED
  }
}

export const login = (username, password) => {
  return (dispatch) => {
    dispatch(loginStarted())
    return fetch('/login', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json; charset=utf-8',
        'Accept': 'application/json'
      },
      body: JSON.stringify({
        uid: username,
        password: password,
      })
    })
    .then(response => response.json())
    .then(data => {
      dispatch(loginSuccess())
      return data.token
    })
    .then(token => {
      ws.connect('ws://127.0.0.1:8080/ws/' + token)
    })
    .then(() => {
      dispatch(wsConnected())
    })
  }
}
