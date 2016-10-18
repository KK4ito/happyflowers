import { combineReducers } from 'redux'
import auth from './auth'
import history from './history'
import settings from './settings'

export default combineReducers({
  auth,
  history,
  settings
})
