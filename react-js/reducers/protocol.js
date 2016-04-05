import {PROTOCOL_RECEIVED} from '../actions'
import Protocols from '../protocols'
const {MessageType, decode} = Protocols

const protocol = (state = {}, action) => {
  switch (action.type) {
    case PROTOCOL_RECEIVED:
      var msg = decode(action.data)
      console.log(msg)
      return state
    default:
      return state
  }
}
export default protocol
