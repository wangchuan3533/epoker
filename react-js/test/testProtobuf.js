var protobuf = require('protobufjs');
var decamelize = require('decamelize');
var camelcase = require('uppercamelcase');
var builder = protobuf.loadJsonFile('../proto/messages.json');
var Message = builder.build('Message');
var MessageType = builder.build('MessageType');
var ActionType = builder.build('ActionType');
var JoinTableReq = builder.build('JoinTableReq')
var x;
for (x in MessageType) {
  console.log(MessageType[x])
  console.log(camelcase(x));
}
for (x in ActionType) {
  console.log(camelcase(x));
}

var msg = new JoinTableReq(123);
console.log(msg);
