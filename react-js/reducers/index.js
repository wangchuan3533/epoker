import { combineReducers } from 'redux'
import auth from './auth'
import connection from './connection'
import game from './game'

const reducers = combineReducers({
  auth,
  connection,
  game
})

export default reducers
