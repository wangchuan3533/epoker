import { combineReducers } from 'redux'
import login from './login'
import connection from './connection'
import protocol from './protocol'

const reducers = combineReducers({
  login,
  connection,
  protocol
})

export default reducers
