import React from 'react'
import { connect } from 'react-redux'
import { browserHistory } from 'react-router'
import { logout } from '../actions'
import './Header.css'

const Header = ({ isLoggedIn, dispatch }) => (
  <header className="site-header">
    <div data-grid>
      <div data-col="L3-4">
        <h1 className="site-title">
          <a href="/">
            happy flowers
          </a>
        </h1>
      </div>
      <div data-col="L1-4">
        <div data-grid>
          {isLoggedIn &&
            <div data-col="1-2">
              <a data-button="block secondary"
                 href="settings">
                Settings
              </a>
            </div>
          }
          <div data-col={isLoggedIn ? '1-2' : ''}>
            {isLoggedIn &&
              <a data-button="block secondary"
                 onClick={() => {
                   dispatch(logout())
                   browserHistory.push('/')
                 }}>
                Logout
              </a>
            }
            {!isLoggedIn &&
              <a data-button="block secondary"
                 onClick={() => browserHistory.push('/login')}>
                Login
              </a>
            }
          </div>
        </div>
      </div>
    </div>
  </header>
)

const mapStateToProps = state => ({
  isLoggedIn: state.auth.jwt
})

export default connect(mapStateToProps)(Header)
