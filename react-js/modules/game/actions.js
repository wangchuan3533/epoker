import Protocols from '../../protocols'
const {Message, MessageType, JoinTableReq} = Protocols

export const PROTOCOL_RECEIVED = 'PROTOCOL_RECEIVED'
export const protocolReceived = (data) => ({type: PROTOCOL_RECEIVED, data})

export const wsConnected = (transport) => (dispatch) => {
  transport.onMessage((data) => {
    dispatch(protocolReceived(data))
  })
  
  // send first protocols
  var req = new JoinTableReq()
  var msg = new Message(MessageType.JOIN_TABLE_REQ, req.toArrayBuffer())
  transport.send(msg.toArrayBuffer())
}
