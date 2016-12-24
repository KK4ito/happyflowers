import { handleActions } from 'redux-actions'
import { List, Map } from 'immutable'
import * as actions from '../actions'

const notifications = handleActions({
  [actions.addNotification]: (state, { payload }) => state.push(Map(payload)),
  [actions.removeNotification]: (state, { payload }) => state.filter(n => n.get('id') !== payload)
}, List())

export default notifications
