import { measurementReceived,
         eventReceived,
         fetchHistorySuccess,
         fetchSettingsSuccess,
         pumpRequested,
         pumpStarted,
         pumpPaused,
         pumpStopped
       } from './actions'

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
          dispatch(measurementReceived(msg.payload))
          break
        case 'eventReceived':
          dispatch(eventReceived(msg.payload))
          break
        case 'historyReceived':
          dispatch(fetchHistorySuccess({ res: { data: msg.payload }}))
          break
        case 'settingsChanged':
          dispatch(fetchSettingsSuccess({ res: { data: msg.payload } }))
          break
        case 'triggerPump':
          dispatch(pumpRequested())

          setTimeout(() => dispatch(pumpStarted()), 3000)
          setTimeout(() => dispatch(pumpPaused()), 8000)
          setTimeout(() => dispatch(pumpStopped()), 16000)
          break
        default:
          break
      }
    } catch (e) {}
  }
}

export default enhanceSockets
