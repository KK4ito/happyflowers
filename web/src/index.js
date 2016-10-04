import React from 'react'
import { render } from 'react-dom'
import { Router, Route, browserHistory } from 'react-router'
import Dashboard from './pages/Dashboard'
import Login from './pages/Login'
import Settings from './pages/Settings'
import './index.css'

render(
  <Router history={browserHistory}>
    <Route path="/"
           component={Dashboard} />
    <Route path="/settings"
           component={Settings} />
    <Route path="/login"
           component={Login} />
  </Router>,
  document.getElementById('root')
)
