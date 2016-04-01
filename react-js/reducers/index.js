import { combineReducers } from 'redux'
import login from './login'
import connection from './connection'

const reducers = combineReducers({
  login,
  connection
})

export default reducers
