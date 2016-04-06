import {LOGIN_STARTED, LOGIN_SUCCESS, LOGIN_FAILED} from '../actions'

const auth = (state = {authorized: false}, action) => {
  switch (action.type) {
    case LOGIN_STARTED:
      console.log('login started')
      return state
    case LOGIN_SUCCESS:
      return {
        ...state,
        authorized: true
      }
    case LOGIN_FAILED:
      console.log('login failed')
      return state
    default:
      return state
  }
}

export default auth
