const connection = (state = {connected: false}, action) => {
  switch (action.type) {
    case 'WS_CONNECTED':
      return {
        ...state,
        connected: true,
      }
    default:
      return state
  }
}

export default connection
