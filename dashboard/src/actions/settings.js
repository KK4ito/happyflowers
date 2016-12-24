import axios from 'axios'
import { createAction } from 'redux-actions'
import { addNotification } from './notifications'
import { events } from '../strings'

export const fetchSettingsRequest = createAction('FETCH_SETTINGS_REQUEST')
export const fetchSettingsSuccess = createAction('FETCH_SETTINGS_SUCCESS')
export const fetchSettingsError = createAction('FETCH_SETTINGS_ERROR')

/**
 * Create a thunk that marks settings fetching as erroneous and creates a
 * notification.
 *
 * @return {function} The function to execute once the action is dispatched.
 */
export const fetchSettingsErrorWithNotification = () => dispatch => {
  dispatch(fetchSettingsError())
  dispatch(addNotification({
    id: 'fse',
    text: 'Could not retrieve settings.',
    type: 'error'
  }))
}

/**
 * Create an API thunk to fetch application settings.
 *
 * @return {object} The object to use for the API thunk.
 */
export const fetchSettings = () => ({
  actions: [ fetchSettingsRequest, fetchSettingsSuccess, fetchSettingsErrorWithNotification ],
  apiCall: () => axios.get('/api/settings')
})

export const submitSettingsRequest = createAction('SUBMIT_SETTINGS_REQUEST')
export const submitSettingsSuccess = createAction('SUBMIT_SETTINGS_SUCCESS')
export const submitSettingsError = createAction('SUBMIT_SETTINGS_ERROR')

/**
 * Create a thunk that marks settings submission as successful and creates a
 * notification.
 *
 * @param {object} payload - The payload to pass to the internal action.
 *
 * @return {function} The function to execute once the action is dispatched.
 */
export const submitSettingsSuccessWithNotification = payload => dispatch => {
  dispatch(submitSettingsSuccess(payload))
  dispatch(addNotification({
    id: 'sss',
    text: 'Settings saved successfully.',
    type: 'success'
  }))
}

/**
 * Create a thunk that marks settings submission as erroneous and creates a
 * notification.
 *
 * @return {function} The function to execute once the action is dispatched.
 */
export const submitSettingsErrorWithNotification = () => dispatch => {
  dispatch(submitSettingsError())
  dispatch(addNotification({
    id: 'sse',
    text: 'Could not save settings.',
    type: 'error'
  }))
}

/**
 * Create an API thunk to store application settings.
 *
 * @param {object} data - The data to submit along with the request.
 * @param {WebSocket} socket - The active WS connection.
 *
 * @return {object} The object to use for the API thunk.
 */
export const submitSettings = (data, socket) => ({
  actions: [ submitSettingsRequest, submitSettingsSuccessWithNotification, submitSettingsErrorWithNotification ],
  apiCall: () => axios.put('/api/settings', data),
  payload: { data },
  successCallback: res => {
    const msg = JSON.stringify({
      type: events.settingsChanged,
      payload: res.data
    })

    if (socket) {
      socket.send(msg)
    }
  }
})
