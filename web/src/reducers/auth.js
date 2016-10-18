import { combineReducers } from 'redux'
import { handleActions } from 'redux-actions'
import * as actions from '../actions'

const jwt = handleActions({
  [actions.loginSuccess]: (_, { payload }) => payload.res.data,
  [actions.loginError]: () => '',
  [actions.logoutRequest]: () => ''
}, window.sessionStorage.getItem('jwt') || '')

const isLoggingIn = handleActions({
  [actions.loginRequest]: () => true,
  [actions.loginSuccess]: () => false,
  [actions.loginError]: () => false,
  [actions.logoutRequest]: () => false
}, false)

export default combineReducers({
  jwt,
  isLoggingIn
})
