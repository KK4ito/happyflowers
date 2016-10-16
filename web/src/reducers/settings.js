import { combineReducers } from 'redux'
import { handleActions } from 'redux-actions'
import * as actions from '../actions'

const data = handleActions({
  [actions.fetchSettingsSuccess]: (_, { payload }) => payload.res.data,
  [actions.submitSettingsSuccess]: (_, { payload }) => payload.res.data
}, {})

const isFetching = handleActions({
  [actions.fetchSettingsRequest]: () => true,
  [actions.fetchSettingsSuccess]: () => false,
  [actions.fetchSettingsError]: () => false
}, false)

const isSubmitting = handleActions({
  [actions.submitSettingsRequest]: () => true,
  [actions.submitSettingsSuccess]: () => false,
  [actions.submitSettingsError]: () => false
}, false)

export default combineReducers({
  data,
  isFetching,
  isSubmitting
})
