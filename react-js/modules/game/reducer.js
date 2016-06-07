import { handleActions } from 'redux-actions'
import {fromJS} from 'immutable'
import Protocols from '../../protocols'
const {MessageType, decode} = Protocols
const {JOIN_TABLE_RES, OTHER_JOIN_TABLE_NTF, OTHER_LEAVE_TABLE_NTF} = MessageType

import {
  PROTOCOL_RECEIVED,
} from './actions'

const initialState = fromJS({
  tableId: -1,
  players: [],
})

export default handleActions({
  [PROTOCOL_RECEIVED]: (state, {data}) => {
    const msg = decode(data)
    console.log(`received protocol: ${msg}`)
    console.log(msg)
    switch (msg.type) {
      case JOIN_TABLE_RES:
        return state.set('tableId', msg.table.id).set('players', fromJS(msg.table.players))
      case OTHER_JOIN_TABLE_NTF:
        return state.update('players', players => players.push(fromJS(msg.player)))
      case OTHER_LEAVE_TABLE_NTF:
        return state.update('players', players => players.filter(player => player.get('id') != msg.player_id))
      default:
        console.log(msg.type)
        return state
    }
  },
  
}, initialState)
