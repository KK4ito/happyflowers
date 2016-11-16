import { combineReducers } from 'redux'
import { handleActions } from 'redux-actions'
import { List, Map } from 'immutable'
import * as actions from '../actions'

/**
 * Creates a reducer to keep track of the snapshot, i.e. the current moisture
 * level of the flower. Default value is 0 if no data is available.
 */
const snapshot = handleActions({
  [actions.fetchHistorySuccess]: (_, { payload }) => {
    const latest = List(payload.res.data.measurements).last()
    return (latest && latest.measurementValue) || 0
  },
  [actions.measurementReceived]: (_, { payload }) => payload.measurementValue
}, 0)

/**
 * Creates a reducer to keep track of historical events, i.e. manual and
 * automatic waterings. Default value is an empty immutable list.
 */
const events = handleActions({
  [actions.fetchHistorySuccess]: (_, { payload }) => List(payload.res.data.events.map(e => Map(e))),
  [actions.eventReceived]: (state, { payload }) => state.push(Map(payload))
}, List())

/**
 * Creates a reducer to keep track of historical measurements, i.e. past
 * moisture levels of the flower. Default value is an empty immutable list.
 * Default value is an empty immutable list.
 */
const measurements = handleActions({
  [actions.fetchHistorySuccess]: (_, { payload }) => List(payload.res.data.measurements.map(m => Map(m))),
  [actions.measurementReceived]: (state, { payload }) => state.push(Map(payload))
}, List())

/**
 * Creates a reducer to keep track of the state of the fetching process. Default
 * value is false, i.e. historical data is not currently being fetched.
 */
const isFetching = handleActions({
  [actions.fetchHistoryRequest]: () => true,
  [actions.fetchHistorySuccess]: () => false,
  [actions.fetchHistoryError]: () => false
}, false)

/**
 * Export a combination of the other reducers.
 */
export default combineReducers({
  snapshot,
  events,
  measurements,
  isFetching
})
