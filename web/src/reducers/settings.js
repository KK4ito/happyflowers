import { combineReducers } from 'redux'

const data = (state = {}, action) => {
  switch (action.type) {
    case 'FETCH_SETTINGS_SUCCESS':
      return action.data
    default:
      return state
  }
}

const isFetching = (state = false, action) => {
  switch (action.type) {
    case 'FETCH_SETTINGS_REQUEST':
      return true
    case 'FETCH_SETTINGS_SUCCESS':
    case 'FETCH_SETTINGS_ERROR':
      return false
    default:
      return state
  }
}

export default combineReducers({
  data,
  isFetching
})
