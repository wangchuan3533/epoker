import {fromJS} from 'immutable'
import {PROTOCOL_RECEIVED} from '../actions'
import Protocols from '../protocols'
const {MessageType, decode} = Protocols
const {JOIN_TABLE_RES, OTHER_JOIN_TABLE_NTF, OTHER_LEAVE_TABLE_NTF} = MessageType
const initialState = fromJS({tableId: -1, players: []})

const game = (state = initialState, action) => {
  switch (action.type) {
    case PROTOCOL_RECEIVED:
      const msg = decode(action.data)
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
    default:
      return state
  }
}
export default game
