import * as actions from './actions'

const enhanceSockets = (socket, dispatch) => {
  // Send all messages that were supposed to be sent before the WS connection
  // could be established.

  socket.onopen = () => {
    socket.send('Connect!')

    if (!window.storedWSMsg) {
      return
    }

    window.storedWSMsg.forEach(m => socket.send(m))
    window.storedWSMsg = null
  }

  // Handle messages based on their type property.

  socket.onmessage = event => {
    try {
      const msg = JSON.parse(event.data)

      switch (msg.type) {
        case 'measurementReceived':
          dispatch(actions.measurementReceived(msg.payload))
          break
        case 'eventReceived':
          dispatch(actions.eventReceived(msg.payload))
          break
        case 'historyReceived':
          dispatch(actions.fetchHistorySuccess({ res: { data: msg.payload }}))
          break
        case 'settingsChanged':
          dispatch(actions.fetchSettingsSuccess({ res: { data: msg.payload } }))
          break
        case 'busy':
          dispatch(actions.busy(msg.payload))
          break
        default:
          break
      }
    } catch (e) {}
  }

  return socket
}

export default enhanceSockets
