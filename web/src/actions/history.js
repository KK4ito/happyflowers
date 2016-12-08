import { createAction } from 'redux-actions'

export const fetchHistorySuccess = createAction('FETCH_HISTORY_SUCCESS')
export const fetchHistoryError = createAction('FETCH_HISTORY_ERROR')

export const measurementReceived = createAction('MEASUREMENT_RECEIVED')
export const eventReceived = createAction('EVENT_RECEIVED')

/**
 * Creates a thunk to trigger the water pump manually.
 *
 * @return {function} The function to execute once the action is dispatched.
 */
export const triggerPump = socket => dispatch => {
  const msg = JSON.stringify({
    type: 'triggerPump'
  })

  if (socket) {
    socket.send(msg)
  }
}

export const busy = createAction('BUSY')
