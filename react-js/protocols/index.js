import protobuf from 'protobufjs'
import camelcase from 'uppercamelcase'

const builder = protobuf.loadJsonFile('/proto/messages.json')
const Message = builder.build('Message')
const MessageType = builder.build('MessageType')
const ActionType = builder.build('ActionType')
const PlayerPb = builder.build('PlayerPb')
const TablePb = builder.build('TablePb')

const Exports = {
  Message,
  MessageType,
  ActionType,
  PlayerPb,
  TablePb
}

const Protocols = {}

for (var messageType in MessageType) {
  const Protocol = builder.build(camelcase(messageType))
  Protocols[MessageType[messageType]] = Protocol
  Exports[camelcase(messageType)] = Protocol
}

Exports.decode = (data) => {
  const msg = Message.decode(data)
  const Protocol = Protocols[msg.type]
  const subMsg = Protocol.decode(msg.data)
  subMsg.type = msg.type
  return subMsg
}

export default Exports
