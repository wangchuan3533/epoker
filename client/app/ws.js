(function() {
  define(function(require) {
    var ws;
    ws = new WebSocket('ws://127.0.0.1:8080/echo');
    ws.binaryType = 'arraybuffer';
    ws.onopen = function() {
      return console.log('connected');
    };
    ws.onmessage = function(evt) {
      return console.log(evt.data);
    };
    ws.onclose = function() {
      return console.log('disconnected');
    };
    return ws;
  });

}).call(this);
