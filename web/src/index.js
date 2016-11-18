import React from 'react'
import { render } from 'react-dom'
import { Router, Route, browserHistory } from 'react-router'
import { Provider } from 'react-redux'
import { createStore, applyMiddleware } from 'redux'
import thunk from 'redux-thunk'
import { List } from 'immutable'
import api from './middleware/api'
import Dashboard from './containers/Dashboard'
import Login from './containers/Login'
import Settings from './containers/Settings'
import reducer from './reducers'
import './index.css'

// By default, the application always contains the API and thunk middlewares.

let middlewares = List.of(api, thunk)

// During development, redux-logger is added to the middleware chain in order to
// log all Redux actions.

if (process.env.NODE_ENV === 'development') {
  middlewares = middlewares.push(require('redux-logger')())
}

const store = createStore(
  reducer,
  applyMiddleware(...middlewares.toJS())
)

render(
  <Provider store={store}>
    <Router history={browserHistory}>
      <Route path="/"
             component={Dashboard} />
      <Route path="/settings"
             component={Settings} />
      <Route path="/login"
             component={Login} />
    </Router>
  </Provider>,
  document.getElementById('root')
)
