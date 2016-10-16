import { combineReducers } from 'redux'
import { handleActions } from 'redux-actions'
import * as actions from '../actions'

const snapshot = handleActions({
  [actions.fetchHistorySuccess]: (_, { payload }) => {
    const latest = payload.res.data.measurements.slice(-1).pop()
    return (latest && latest.measurementValue) || 0
  },
  [actions.measurementReceived]: (_, { measurement }) => measurement.measurementValue
}, 0)

const events = handleActions({
  [actions.fetchHistorySuccess]: (_, { payload }) => payload.res.data.events,
  [actions.eventReceived]: (state, { payload }) => [...state, payload]
}, [])

const measurements = handleActions({
  [actions.fetchHistorySuccess]: (_, { payload }) => payload.res.data.measurements,
  [actions.measurementReceived]: (state, { payload }) => [...state, payload]
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
