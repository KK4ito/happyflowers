import axios from 'axios'
import { createAction } from 'redux-actions'
import { browserHistory } from 'react-router'
import { addNotification } from './notifications'

export const loginRequest = createAction('LOGIN_REQUEST')
export const loginSuccess = createAction('LOGIN_SUCCESS')
export const loginError = createAction('LOGIN_ERROR')

/**
 * Create a thunk that marks login attempts as erroneous and creates a
 * notification.
 *
 * @return {function} The function to execute once the action is dispatched.
 */
export const loginErrorWithNotification = () => dispatch => {
  dispatch(loginError())
  dispatch(addNotification({
    id: 'le',
    text: 'The server was unable to verify your password.',
    type: 'error'
  }))
}

/**
 * Create an API thunk to attempt user login.
 *
 * @param {FormData} data - The FormData to submit along with the request.
 *
 * @return {object} The object to use for the API thunk.
 */
export const login = data => ({
  actions: [ loginRequest, loginSuccess, loginErrorWithNotification ],
  apiCall: () => axios.post('/api/auth', data),
  successCallback: res => {
    window.sessionStorage.setItem('jwt', res.data.token)
    browserHistory.push('/')
  }
})

export const logoutRequest = createAction('LOGOUT_REQUEST')

/**
 * Creates a thunk to attempt user logout. Simply removes the JWT entry from the
 * current session.
 *
 * @return {function} The function to execute once the action is dispatched.
 */
export const logout = () => dispatch => {
  window.sessionStorage.removeItem('jwt')
  browserHistory.push('/')

  dispatch(logoutRequest())

  dispatch(addNotification({
    id: 'l',
    text: 'Successfully logged out. See you next time.',
    type: 'success'
  }))
}
