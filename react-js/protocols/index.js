import protobuf from 'protobufjs'
const builder = protobuf.loadJsonFile('/proto/messages.json')

export const Message = builder.build('Message')
export const TablePb = builder.build('TablePb')
export const PlayerPb = builder.build('PlayerPb')
export const MessageType = builder.lookup('Message.Type').object
export const ActionType = builder.lookup('ActionReq.Action').object

export const JoinTableReq = builder.build('JoinTableReq')
export const JoinTableRes = builder.build('JoinTableRes')

export const LeaveTableReq = builder.build('LeaveTableReq')
export const LeaveTableRes = builder.build('LeaveTableRes')

export const ListTableReq = builder.build('ListTableReq')
export const ListTableRes = builder.build('ListTableRes')

export const LeaveGameReq = builder.build('LeaveGameReq')
export const LeaveGameRes = builder.build('LeaveGameRes')

export const ActionReq = builder.build('ActionReq')
export const ActionRes = builder.build('ActionRes')

export const onMessage = (data, dispatch) => {
  const msg = Message.decode(data)
  var res
  switch(msg.type) {
    case MessageType.JOIN_TABLE_RES:
      console.log('JOIN_TABLE_RES')
      res = JoinTableRes.decode(msg.data)
      return dispatch(res)
    case MessageType.LEAVE_TABLE_RES:
      console.log('LEAVE_TABLE_RES')
      res = LeaveTableRes.decode(msg.data)
      return dispatch(res)
    case MessageType.LIST_TABLE_RES:
      console.log('LIST_TABLE_RES')
      res = ListTableRes.decode(msg.data)
      return dispatch(res)
    case MessageType.LEAVE_GAME_RES:
      console.log('LEAVE_GAME_RES')
      res = LeaveGameRes.decode(msg.data)
      return dispatch(res)
    case MessageType.ACTION_RES:
      console.log('ACTION_RES')
      res = ActionRes.decode(msg.data)
      return dispatch(res)
    default:
      return dispatch({type: 'unknown protocol'})
  }
}
