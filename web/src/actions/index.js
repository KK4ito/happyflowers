import axios from 'axios'

export const requestFetchSettings = () => ({
  type: 'FETCH_SETTINGS_REQUEST'
})

export const succeedFetchSettings = (data) => ({
  type: 'FETCH_SETTINGS_SUCCESS',
  data
})

export const failFetchSettings = () => ({
  type: 'FETCH_SETTINGS_ERROR'
})

export const fetchSettings = () => (dispatch) => {
  dispatch(requestFetchSettings())

  return axios.get('http://localhost:5000/settings')
    .then(res => {
      dispatch(succeedFetchSettings(res.data))
    })
    .catch(err => {
      dispatch(failFetchSettings())
    })
}

export const requestSubmitSettings = () => ({
  type: 'SUBMIT_SETTINGS_REQUEST'
})

export const succeedSubmitSettings = (data) => ({
  type: 'SUBMIT_SETTINGS_SUCCESS',
  data
})

export const failSubmitSettings = () => ({
  type: 'SUBMIT_SETTINGS_ERROR'
})

export const submitSettings = (data) => (dispatch) => {
  dispatch(requestSubmitSettings())

  return axios.put('http://localhost:5000/settings', data)
    .then(res => {
      dispatch(succeedSubmitSettings(res.data))
    })
    .catch(err => {
      dispatch(failSubmitSettings())
    })
}

export const requestFetchHistory = () => ({
  type: 'FETCH_HISTORY_REQUEST'
})

export const succeedFetchHistory = (measurements, events) => ({
  type: 'FETCH_HISTORY_SUCCESS',
  measurements,
  events
})

export const failFetchHistory = () => ({
  type: 'FETCH_HISTORY_ERROR'
})

export const fetchHistory = () => (dispatch) => {
  dispatch(requestFetchHistory())

  return axios.get('http://localhost:5000/history')
    .then(res => {
      dispatch(succeedFetchHistory(res.data.measurements, res.data.events))
    })
    .catch(err => {
      dispatch(failFetchHistory())
    })
}
