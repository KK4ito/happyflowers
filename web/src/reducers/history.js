import { combineReducers } from 'redux'

const snapshot = (state = 0, action) => {
  switch (action.type) {
    case 'FETCH_HISTORY_SUCCESS':
      return action.measurements.slice(-1).pop().measurementValue
    case 'MEASUREMENT_RECEIVED':
      return action.measurement.measurementValue
    default:
      return state
  }
}

const events = (state = [], action) => {
  switch (action.type) {
    case 'FETCH_HISTORY_SUCCESS':
      return action.events
    case 'EVENT_RECEIVED':
      return [...state, action.event]
    default:
      return state
  }
}

const measurements = (state = [], action) => {
  switch (action.type) {
    case 'FETCH_HISTORY_SUCCESS':
      return action.measurements
    case 'MEASUREMENT_RECEIVED':
      return [...state, action.measurement]
    default:
      return state
  }
}

const isFetching = (state = false, action) => {
  switch (action.type) {
    case 'FETCH_HISTORY_REQUEST':
      return true
    case 'FETCH_HISTORY_SUCCESS':
    case 'FETCH_HISTORY_ERROR':
      return false
    default:
     return state
  }
}

export default combineReducers({
  snapshot,
  events,
  measurements,
  isFetching
})
