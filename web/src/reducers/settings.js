import { combineReducers } from 'redux'
import { handleActions } from 'redux-actions'
import { Map } from 'immutable'
import * as actions from '../actions'

/**
 * Creates a reducer to keep track of the application settings data. Default
 * value is an empty immutable map.
 */
const data = handleActions({
  [actions.fetchSettingsSuccess]: (_, { payload }) => Map(payload.res.data),
  [actions.submitSettingsSuccess]: (_, { payload }) => Map(payload.res.data)
}, Map())

/**
 * Creates a reducer to keep track of the state of the fetching process. Default
 * value is false, i.e. application settings data is not currently being
 * fetched.
 */
const isFetching = handleActions({
  [actions.fetchSettingsRequest]: () => true,
  [actions.fetchSettingsSuccess]: () => false,
  [actions.fetchSettingsError]: () => false
}, false)

/**
 * Creates a reducer to keep track of the state of the submission process.
 * Default value is false, i.e. application settings data is not currently being
 * submitted.
 */
const isSubmitting = handleActions({
  [actions.submitSettingsRequest]: () => true,
  [actions.submitSettingsSuccess]: () => false,
  [actions.submitSettingsError]: () => false
}, false)

/**
 * Export a combination of the other reducers.
 */
export default combineReducers({
  data,
  isFetching,
  isSubmitting
})
