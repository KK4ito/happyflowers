import React from 'react'
import { render } from 'react-dom'
import { Router, Route, browserHistory } from 'react-router'
import { Provider } from 'react-redux'
import { createStore, applyMiddleware } from 'redux'
import thunk from 'redux-thunk'
import Dashboard from './pages/Dashboard'
import Login from './pages/Login'
import Settings from './pages/Settings'
import reducer from './reducers'
import './index.css'

const middlewares = [thunk]

if (process.env.NODE_ENV === 'development') {
  const logger = require('redux-logger')()
  middlewares.push(logger)
}

const store = createStore(
  reducer,
  applyMiddleware(...middlewares)
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
