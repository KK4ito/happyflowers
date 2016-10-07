import React from 'react'
import { render } from 'react-dom'
import { Router, Route, browserHistory } from 'react-router'
import { Provider } from 'react-redux'
import { createStore, applyMiddleware } from 'redux'
import createLogger from 'redux-logger'
import thunk from 'redux-thunk'
import Dashboard from './pages/Dashboard'
import Login from './pages/Login'
import Settings from './pages/Settings'
import reducer from './reducers'
import './index.css'

const logger = createLogger()

const store = createStore(
  reducer,
  applyMiddleware(thunk, logger)
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
