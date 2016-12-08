import { handleActions } from 'redux-actions'
import { List } from 'immutable'
import * as actions from '../actions'

const notifications = handleActions({
  [actions.addNotification]: (state, { payload }) => state.push(payload),
  [actions.removeNotification]: (state, { payload }) => state.filter(n => n.id !== payload)
}, List())

export default notifications
