import Protocols from '../../protocols'
const {Message, MessageType, JoinTableReq, ActionReq, ActionType} = Protocols

export const PROTOCOL_RECEIVED = 'PROTOCOL_RECEIVED'
export const protocolReceived = (data) => ({type: PROTOCOL_RECEIVED, data})

export const WS_CONNECTED = 'WS_CONNECTED'
export const wsConnected = (transport) => ({type: WS_CONNECTED, transport})

export const registerProtocols = () => (dispatch, getState) => {
  const transport = getState().game.get('transport')
  
  transport.onMessage((data) => {
    dispatch(protocolReceived(data))
  })
  
  // send first protocols
  const req = new JoinTableReq()
  const msg = new Message({type: MessageType.JOIN_TABLE_REQ, data: req.toArrayBuffer()})
  transport.send(msg.toArrayBuffer())
}

export const RAISE = 'RAISE'
export const raise = (amount) => (dispatch, getState) => {
  const transport = getState().game.get('transport')
  const req = new ActionReq({action: ActionType.ACTION_RAISE, amount})
  const msg = new Message({type: MessageType.ACTION_REQ, data: req.toArrayBuffer()})
  transport.send(msg.toArrayBuffer())
}

export const FOLD = 'FOLD'
export const fold = () => (dispatch, getState) => {
  const transport = getState().game.get('transport')
  const req = new ActionReq({action: ActionType.ACTION_FOLD, amount: 0})
  const msg = new Message({type: MessageType.ACTION_REQ, data: req.toArrayBuffer()})
  transport.send(msg.toArrayBuffer())
}
