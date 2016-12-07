import React from 'react'
import { connect } from 'react-redux'
import Alert from 'react-s-alert'
import Loader from '../components/Loader'
import { login } from '../actions'
import './Login.css'

/**
 * Class representing the login screen of the application.
 *
 * @extends React.Component
 */
class Login extends React.Component {
  static propTypes = {
    jwt: React.PropTypes.string.isRequired,
    isLoggingIn: React.PropTypes.bool,
    dispatch: React.PropTypes.func.isRequired,
    socket: React.PropTypes.object
  }

  /**
   * Create a Login component. Sets initial state and binds class methods.
   *
   * @param {object} props - Standard react props to be passed to the parent
   *                         constructor.
   */
  constructor(props) {
    super(props)

    this.state = {
      password: ''
    }

    this.login = this.login.bind(this)
  }

  /**
   * Lifecycle method that is executed whenever the component is mounted.
   * Focuses the password input.
   */
  componentDidMount() {
    this.password.focus()
  }

  /**
   * Attempt to log in the user.
   *
   * @param {object} event - Submit event from the log in form. Used to prevent
   *                         the default behaviour in favour of AJAX
   *                         functionality.
   */
  login(event) {
    event.preventDefault()

    // Redirect the user to the dashboard upon successful login, otherwise show
    // an error message.

    const data = {
      password: this.state.password
    }

    this.props.dispatch(login(data))
      .catch(() => Alert.error('The server was unable to verify your password.'))
  }

  /**
   * Renders the component.
   *
   * @return {string} - HTML markup for the component.
   */
  render() {
    return (
      <main className="site">
        <form className="login">
          <Loader loading={this.props.isLoggingIn} />
          <h1 className="form-title">
            <a href="/dashboard">
              happy flowers
            </a>
          </h1>
          <input className="text-input full-width spaced"
                 ref={n => this.password = n}
                 type="password"
                 value={this.state.password}
                 onChange={ev => this.setState({ password: ev.target.value })}
                 placeholder="Enter the password" />
          <input data-button="block"
                 disabled={!this.state.password.length || this.props.isLoggingIn}
                 type="submit"
                 value={this.props.isLoggingIn ? 'Signing in…' : 'Sign In'}
                 onClick={this.login} />
        </form>
        <Alert stack={{limit: 3}}
               timeout={2000}
               effect="slide"
               position="bottom" />
      </main>
    )
  }
}

/**
 * Map Redux state to React props for the Login component.
 *
 * @param {object} state - The Redux state, injected by the <code>connect</code>
 *                         function.
 */
const mapStateToProps = state => ({
  jwt: state.auth.jwt,
  isLoggingIn: state.auth.isLoggingIn
})

export default connect(mapStateToProps)(Login)
