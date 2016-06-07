import { handleActions } from 'redux-actions'
import {fromJS} from 'immutable'
import {
  SHOW_LOGIN_DIALOG,
  HIDE_LOGIN_DIALOG,
  LOGIN_SUCESS,
} from './actions'

const initialState = fromJS({
  showLoginDialog: true,
})

export default handleActions({
  [SHOW_LOGIN_DIALOG]: (state, action) => state.set('showLoginDialog', true),
  [HIDE_LOGIN_DIALOG]: (state, action) => state.set('showLoginDialog', false),
  [LOGIN_SUCESS]: (state, action) => state.set('showLoginDialog', false),
}, initialState)
