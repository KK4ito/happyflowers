import { combineReducers } from 'redux'
import auth from './auth'
import history from './history'
import settings from './settings'

/**
 * Export a combination of all other reducers. This acts as the root reducer.
 */
export default combineReducers({
  auth,
  history,
  settings
})
