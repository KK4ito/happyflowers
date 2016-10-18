import { combineReducers } from 'redux'
import { handleActions } from 'redux-actions'
import * as actions from '../actions'

const jwt = handleActions({
  [actions.loginSuccess]: (_, { payload }) => {
    window.sessionStorage.setItem('jwt', payload.res.data)
    return payload.res.data
  },
  [actions.loginError]: () => '',
  [actions.logoutRequest]: () => {
    window.sessionStorage.removeItem('jwt')
    return ''
  }
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
