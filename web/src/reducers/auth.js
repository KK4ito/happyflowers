import { combineReducers } from 'redux'
import { handleActions } from 'redux-actions'
import * as actions from '../actions'

/**
 * Creates a reducer to keep track of the value of the authentication JWT.
 * Default value is either the stored JWT from the current session or an empty
 * string.
 */
const jwt = handleActions({
  [actions.loginSuccess]: (_, { payload }) => payload.res.data,
  [actions.loginError]: () => '',
  [actions.logoutRequest]: () => ''
}, window.sessionStorage.getItem('jwt') || '')

/**
 * Creates a reducer to keep track of the state of the log in process. Default
 * value is false, i.e. the user is not currently logging in.
 */
const isLoggingIn = handleActions({
  [actions.loginRequest]: () => true,
  [actions.loginSuccess]: () => false,
  [actions.loginError]: () => false,
  [actions.logoutRequest]: () => false
}, false)

/**
 * Export a combination of the other reducers.
 */
export default combineReducers({
  jwt,
  isLoggingIn
})
