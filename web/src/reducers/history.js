import { combineReducers } from 'redux'
import { handleActions } from 'redux-actions'
import { List, Map, fromJS } from 'immutable'
import * as actions from '../actions'
import { measurementKinds } from '../strings'

/**
 * Creates a reducer to keep track of the snapshot, i.e. the current moisture
 * level of the flower. Default value is 0 if no data is available.
 */
const snapshot = handleActions({
  [actions.fetchHistorySuccess]: (state, { payload }) => {
    const mes = List(payload.res.data.measurements)

    const latestMoisture = mes.filter(m => m.measurementKind === measurementKinds.moisture).last()
    const latestTemperature = mes.filter(m => m.measurementKind === measurementKinds.temperature).last()

    return state
      .set(measurementKinds.moisture, (latestMoisture && latestMoisture.measurementValue) || 0)
      .set(measurementKinds.temperature, (latestTemperature && latestTemperature.measurementValue) || 0)
  },
  [actions.measurementReceived]: (state, { payload }) => state.set(payload.measurementKind, payload.measurementValue)
}, fromJS({
  [measurementKinds.moisture]: 0,
  [measurementKinds.temperature]: 0
}))

/**
 * Creates a reducer to keep track of historical events, i.e. manual and
 * automatic waterings. Default value is an empty immutable list.
 */
const events = handleActions({
  [actions.fetchHistorySuccess]: (state, { payload }) => List(payload.res.data.events.map(e => Map(e))).concat(state),
  [actions.eventReceived]: (state, { payload }) => state.push(Map(payload))
}, List())

/**
 * Creates a reducer to keep track of historical measurements, i.e. past
 * moisture levels of the flower. Default value is an empty immutable list.
 * Default value is an empty immutable list.
 */
const measurements = handleActions({
  [actions.fetchHistorySuccess]: (state, { payload }) => List(payload.res.data.measurements.map(m => Map(m))).concat(state),
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
