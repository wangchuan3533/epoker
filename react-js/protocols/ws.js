
const ws = {
  sock: null,
  connect: (url) => {
    return new Promise((resolve, reject) => {
      var sock = new WebSocket(url)
      sock.binaryType = 'arraybuffer'
      sock.onopen = () => {
        console.log('connected')
        ws.sock = sock
        resolve()
      }
      sock.onerror = () => {
        console.log('connect error')
        reject(new Error('websocket connection error'))
      }
      sock.onmessage = (evt) => {
        console.log(evt.data)
      }
      sock.onclose = () => {
        console.log('closed')
      }
    })
  },
  send: (data) => {
    ws.sock.send(data)
  },
  close: () => {
    ws.sock.close()
  }
}

export default ws
