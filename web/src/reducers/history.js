import { combineReducers } from 'redux'
import { handleActions } from 'redux-actions'
import * as actions from '../actions'

const snapshot = handleActions({
  [actions.fetchHistorySuccess]: (state, action) => {
    const latest = action.payload.res.data.measurements.slice(-1).pop()
    return (latest && latest.measurementValue) || 0
  },
  [actions.measurementReceived]: (state, action) => action.measurement.measurementValue
}, 0)

const events = handleActions({
  [actions.fetchHistorySuccess]: (state, action) => action.payload.res.data.events,
  [actions.eventReceived]: (state, action) => [...state, action.payload]
}, [])

const measurements = handleActions({
  [actions.fetchHistorySuccess]: (state, action) => action.payload.res.data.measurements,
  [actions.measurementReceived]: (state, action) => [...state, action.payload]
}, [])

const isFetching = handleActions({
  [actions.fetchHistoryRequest]: () => true,
  [actions.fetchHistorySuccess]: () => false,
  [actions.fetchHistoryError]: () => false
}, false)

export default combineReducers({
  snapshot,
  events,
  measurements,
  isFetching
})
