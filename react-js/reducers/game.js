import {PROTOCOL_RECEIVED} from '../actions'
import Protocols from '../protocols'
const {MessageType, decode} = Protocols
const {JOIN_TABLE_RES, OTHER_JOIN_TABLE_NTF} = MessageType

const game = (state = {}, action) => {
  switch (action.type) {
    case PROTOCOL_RECEIVED:
      const msg = decode(action.data)
      switch (msg.type) {
        case JOIN_TABLE_RES:
          return {
            ...state,
            tableId: msg.table.id,
            players: msg.table.players
          }
        case OTHER_JOIN_TABLE_NTF:
          return {
            ...state,
            players: state.players.concat(msg.player)
          }
        default:
          console.log(msg.type)
          return state
      }
    default:
      return state
  }
}
export default game
