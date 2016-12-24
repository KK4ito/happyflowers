import axios from 'axios'
import { createAction } from 'redux-actions'
import { events } from '../strings'

export const fetchHistoryRequest = createAction('FETCH_HISTORY_REQUEST')
export const fetchHistorySuccess = createAction('FETCH_HISTORY_SUCCESS')
export const fetchHistoryError = createAction('FETCH_HISTORY_ERROR')

/**
 * Create an API thunk to fetch historical data.
 *
 * @return {object} The object to use for the API thunk.
 */
export const fetchHistory = () => ({
  actions: [ fetchHistoryRequest, fetchHistorySuccess, fetchHistoryError ],
  apiCall: () => axios.get('/api/history')
})

export const measurementReceived = createAction('MEASUREMENT_RECEIVED')
export const eventReceived = createAction('EVENT_RECEIVED')

/**
 * Creates a thunk to trigger the water pump manually.
 *
 * @param {WebSocket} socket - The active WS connection.
 *
 * @return {function} The function to execute once the action is dispatched.
 */
export const triggerPump = socket => dispatch => {
  const msg = JSON.stringify({
    type: events.triggerPump
  })

  if (socket) {
    socket.send(msg)
  }
}

export const busy = createAction('BUSY')
