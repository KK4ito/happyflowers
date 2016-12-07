import React from 'react'
import { connect } from 'react-redux'
import { Link } from 'react-router'
import { logout } from '../actions'
import './Header.css'

/**
 * Functional component representing the website header containing the document
 * title and navigation.
 *
 * @param {object} props - Standard React props, destructured to only get the
 *                         isLoggedIn and dispatch props.
 *
 * @return {string} - HTML markup for the component.
 */
const Header = ({ isLoggedIn, dispatch }) => (
  <header className="site-header">
    <div data-grid>
      <div data-col="L3-4">
        <h1 className="site-title">
          <Link to="/dashboard">
            happy flowers
          </Link>
        </h1>
      </div>
      <div data-col="L1-4">
        <div data-grid>
          {isLoggedIn &&
            <div data-col="1-2">
              <Link data-button="block"
                    to="/settings">
                Settings
              </Link>
            </div>
          }
          <div data-col={isLoggedIn ? '1-2' : ''}>
            {isLoggedIn &&
              <button data-button="block"
                      onClick={() => dispatch(logout())}>
                Logout
              </button>
            }
            {!isLoggedIn &&
              <Link data-button="block"
                    to="/login">
                Login
              </Link>
            }
          </div>
        </div>
      </div>
    </div>
  </header>
)

Header.propTypes = {
  isLoggedIn: React.PropTypes.bool,
  dispatch: React.PropTypes.func.isRequired
}

/**
 * Map Redux state to React props for the Header component.
 *
 * @param {object} state - The Redux state, injected by the <code>connect</code>
 *                         function.
 */
const mapStateToProps = state => ({
  isLoggedIn: !!state.auth.jwt
})

export default connect(mapStateToProps)(Header)
