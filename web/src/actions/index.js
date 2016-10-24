import axios from 'axios'
import { createAction } from 'redux-actions'

export const fetchSettingsRequest = createAction('FETCH_SETTINGS_REQUEST')
export const fetchSettingsSuccess = createAction('FETCH_SETTINGS_SUCCESS')
export const fetchSettingsError = createAction('FETCH_SETTINGS_ERROR')

export const fetchSettings = () => ({
  actions: [ fetchSettingsRequest, fetchSettingsSuccess, fetchSettingsError ],
  apiCall: () => axios.get(`${process.env.NODE_ENV === 'development' ? 'http://localhost:5000' : ''}/api/settings`)
})

export const submitSettingsRequest = createAction('SUBMIT_SETTINGS_REQUEST')
export const submitSettingsSuccess = createAction('SUBMIT_SETTINGS_SUCCESS')
export const submitSettingsError = createAction('SUBMIT_SETTINGS_ERROR')

export const submitSettings = data => ({
  actions: [ submitSettingsRequest, submitSettingsSuccess, submitSettingsError ],
  apiCall: () => axios.put(`${process.env.NODE_ENV === 'development' ? 'http://localhost:5000' : ''}/api/settings`, data),
  payload: { data }
})

export const fetchHistoryRequest = createAction('FETCH_HISTORY_REQUEST')
export const fetchHistorySuccess = createAction('FETCH_HISTORY_SUCCESS')
export const fetchHistoryError = createAction('FETCH_HISTORY_ERROR')

export const fetchHistory = () => ({
  actions: [ fetchHistoryRequest, fetchHistorySuccess, fetchHistoryError ],
  apiCall: () => axios.get(`${process.env.NODE_ENV === 'development' ? 'http://localhost:5000' : ''}/api/history`)
})

export const loginRequest = createAction('LOGIN_REQUEST')
export const loginSuccess = createAction('LOGIN_SUCCESS')
export const loginError = createAction('LOGIN_ERROR')

export const login = data => ({
  actions: [ loginRequest, loginSuccess, loginError ],
  apiCall: () => axios.post(`${process.env.NODE_ENV === 'development' ? 'http://localhost:5000' : ''}/api/auth`, data),
  successCallback: res => window.sessionStorage.setItem('jwt', res.data)
})

export const logoutRequest = createAction('LOGOUT_REQUEST')

export const logout = () => dispatch => {
  window.sessionStorage.removeItem('jwt')
  dispatch(logoutRequest())
}
