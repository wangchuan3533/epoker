import { handleActions } from 'redux-actions'
import {fromJS} from 'immutable'
import Protocols from '../../protocols'
const {MessageType, ActionType, decode} = Protocols

import {
  PROTOCOL_RECEIVED,
  WS_CONNECTED,
} from './actions'

const initialState = fromJS({
  tableId: -1,
  players: [],
  started: false,
  transport: null,
})

export default handleActions({
  [WS_CONNECTED]: (state, {transport}) => state.set('transport', transport),
  [PROTOCOL_RECEIVED]: (state, {data}) => {
    const msg = decode(data)
    console.log(`received protocol: ${msg}`)
    console.log(msg)
    switch (msg.type) {
      case MessageType.JOIN_TABLE_RES:
        return state.set('tableId', msg.table.id).set('players', fromJS(msg.table.players))
      case MessageType.OTHER_JOIN_TABLE_NTF:
        return state.update('players', players => players.push(fromJS(msg.player)))
      case MessageType.OTHER_LEAVE_TABLE_NTF:
        return state.update('players', players => players.filter(player => player.get('id') != msg.player_id))
      case MessageType.GAME_STARTED_NTF:
        return state.set('started', true)
      case MessageType.GAME_FINISHED_NTF:
        return state.set('started', false )
      default:
        console.log(msg.type)
        return state
    }
  },
  
}, initialState)
