import {PROTOCOL_RECEIVED} from '../actions'
import {Message, MessageType, ListTableReq} from '../protocols'

const protocol = (state = {}, action) => {
  switch (action.type) {
    case PROTOCOL_RECEIVED:
      var msg = Message.decode(action.data)
      console.log(msg.type)
      return state
    default:
      return state
  }
}
export default protocol
