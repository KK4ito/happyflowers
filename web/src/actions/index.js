import axios from 'axios'
import { createAction } from 'redux-actions'

/**
 * Creates a FETCH_SETTINGS_REQUEST action with no specified payload
 * transformation.
 */
export const fetchSettingsRequest = createAction('FETCH_SETTINGS_REQUEST')

/**
 * Creates a FETCH_SETTINGS_SUCCESS action with no specified payload
 * transformation.
 */
export const fetchSettingsSuccess = createAction('FETCH_SETTINGS_SUCCESS')

/**
 * Creates a FETCH_SETTINGS_ERROR action with no specified payload
 * transformation.
 */
export const fetchSettingsError = createAction('FETCH_SETTINGS_ERROR')

/**
 * Create an API thunk to fetch application settings.
 *
 * @return {object} The object to use for the API thunk.
 */
export const fetchSettings = () => ({
  actions: [ fetchSettingsRequest, fetchSettingsSuccess, fetchSettingsError ],
  apiCall: () => axios.get(`${process.env.NODE_ENV === 'development' ? 'http://localhost:5000' : ''}/api/settings`)
})

/**
 * Creates a SUBMIT_SETTINGS_REQUEST action with no specified payload
 * transformation.
 */
export const submitSettingsRequest = createAction('SUBMIT_SETTINGS_REQUEST')

/**
 * Creates a SUBMIT_SETTINGS_SUCCESS action with no specified payload
 * transformation.
 */
export const submitSettingsSuccess = createAction('SUBMIT_SETTINGS_SUCCESS')

/**
 * Creates a SUBMIT_SETTINGS_ERROR action with no specified payload
 * transformation.
 */
export const submitSettingsError = createAction('SUBMIT_SETTINGS_ERROR')

/**
 * Create an API thunk to store application settings.
 *
 * @param {FormData} data - The FormData to submit along with the request.
 *
 * @return {object} The object to use for the API thunk.
 */
export const submitSettings = data => ({
  actions: [ submitSettingsRequest, submitSettingsSuccess, submitSettingsError ],
  apiCall: () => axios.put(`${process.env.NODE_ENV === 'development' ? 'http://localhost:5000' : ''}/api/settings`, data),
  payload: { data }
})

/**
 * Creates a FETCH_HISTORY_REQUEST action with no specified payload
 * transformation.
 */
export const fetchHistoryRequest = createAction('FETCH_HISTORY_REQUEST')

/**
 * Creates a FETCH_HISTORY_SUCCESS action with no specified payload
 * transformation.
 */
export const fetchHistorySuccess = createAction('FETCH_HISTORY_SUCCESS')

/**
 * Creates a FETCH_HISTORY_ERROR action with no specified payload
 * transformation.
 */
export const fetchHistoryError = createAction('FETCH_HISTORY_ERROR')

/**
 * Create an API thunk to fetch historical data.
 *
 * @return {object} The object to use for the API thunk.
 */
export const fetchHistory = () => ({
  actions: [ fetchHistoryRequest, fetchHistorySuccess, fetchHistoryError ],
  apiCall: () => axios.get(`${process.env.NODE_ENV === 'development' ? 'http://localhost:5000' : ''}/api/history`)
})

/**
 * Creates a LOGIN_REQUEST action with no specified payload
 * transformation.
 */
export const loginRequest = createAction('LOGIN_REQUEST')

/**
 * Creates a LOGIN_SUCCESS action with no specified payload
 * transformation.
 */
export const loginSuccess = createAction('LOGIN_SUCCESS')

/**
 * Creates a LOGIN_ERROR action with no specified payload
 * transformation.
 */
export const loginError = createAction('LOGIN_ERROR')

/**
 * Create an API thunk to attempt user login.
 *
 * @param {FormData} data - The FormData to submit along with the request.
 *
 * @return {object} The object to use for the API thunk.
 */
export const login = data => ({
  actions: [ loginRequest, loginSuccess, loginError ],
  apiCall: () => axios.post(`${process.env.NODE_ENV === 'development' ? 'http://localhost:5000' : ''}/api/auth`, data),
  successCallback: res => window.sessionStorage.setItem('jwt', res.data)
})

/**
 * Creates a LOGOUT_REQUEST action with no specified payload
 * transformation.
 */
export const logoutRequest = createAction('LOGOUT_REQUEST')

/**
 * Creates a thunk to attempt user logout. Simply removes the JWT entry from the
 * current session.
 *
 * @return {function} The function to execute once the action is dispatched.
 */
export const logout = () => dispatch => {
  window.sessionStorage.removeItem('jwt')
  dispatch(logoutRequest())
}
