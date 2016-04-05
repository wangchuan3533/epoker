var protobuf = require('protobufjs');
var decamelize = require('decamelize');
var camelcase = require('uppercamelcase');
var builder = protobuf.loadJsonFile('../proto/messages.json');
var Message = builder.build('Message');
var MessageType = Message.Type;
var x;
for (x in MessageType) {
  console.log(camelcase(x));
}
