import { combineReducers } from 'redux'
import { handleActions } from 'redux-actions'
import * as actions from '../actions'

/**
 * Creates a reducer to keep track of the value of the authentication JWT.
 * Default value is either the stored JWT from the current session or an empty
 * string.
 */
const pumpState = handleActions({
  [actions.pumpRequested]: () => 1,
  [actions.pumpStarted]: () => 2,
  [actions.pumpPaused]: () => 3,
  [actions.pumpStopped]: () => 0
}, 0)

/**
 * Export a combination of the other reducers.
 */
export default combineReducers({
  pumpState
})
