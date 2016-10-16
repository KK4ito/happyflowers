import { combineReducers } from 'redux'
import { handleActions } from 'redux-actions'
import { List, Map } from 'immutable'
import * as actions from '../actions'

const snapshot = handleActions({
  [actions.fetchHistorySuccess]: (_, { payload }) => {
    const latest = List(payload.res.data.measurements).last()
    return (latest && latest.measurementValue) || 0
  },
  [actions.measurementReceived]: (_, { measurement }) => measurement.measurementValue
}, 0)

const events = handleActions({
  [actions.fetchHistorySuccess]: (_, { payload }) => List(payload.res.data.events.map(e => Map(e))),
  [actions.eventReceived]: (state, { payload }) => state.push(Map(payload))
}, List())

const measurements = handleActions({
  [actions.fetchHistorySuccess]: (_, { payload }) => List(payload.res.data.measurements.map(m => Map(m))),
  [actions.measurementReceived]: (state, { payload }) => state.push(Map(payload))
}, List())

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
