import {PROTOCOL_RECEIVED} from '../actions'
import Protocols from '../protocols'
const {MessageType, decode} = Protocols
const {JOIN_TABLE_RES} = MessageType

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
      }
    default:
      return state
  }
}
export default game
