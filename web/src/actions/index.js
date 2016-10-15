import axios from 'axios'
import { createAction } from 'redux-actions'

export const fetchSettingsRequest = createAction('FETCH_SETTINGS_REQUEST')
export const fetchSettingsSuccess = createAction('FETCH_SETTINGS_SUCCESS')
export const fetchSettingsError = createAction('FETCH_SETTINGS_ERROR')

export const fetchSettings = () => ({
  actions: [ fetchSettingsRequest, fetchSettingsSuccess, fetchSettingsError ],
  apiCall: () => axios.get('http://localhost:5000/settings')
})

export const submitSettingsRequest = createAction('SUBMIT_SETTINGS_REQUEST')
export const submitSettingsSuccess = createAction('SUBMIT_SETTINGS_SUCCESS')
export const submitSettingsError = createAction('SUBMIT_SETTINGS_ERROR')

export const submitSettings = data => ({
  actions: [ submitSettingsRequest, submitSettingsSuccess, submitSettingsError ],
  apiCall: () => axios.put('http://localhost:5000/settings', data),
  payload: { data }
})

export const fetchHistoryRequest = createAction('FETCH_HISTORY_REQUEST')
export const fetchHistorySuccess = createAction('FETCH_HISTORY_SUCCESS')
export const fetchHistoryError = createAction('FETCH_HISTORY_ERROR')

export const fetchHistory = () => ({
  actions: [ fetchHistoryRequest, fetchHistorySuccess, fetchHistoryError ],
  apiCall: () => axios.get('http://localhost:5000/history')
})
