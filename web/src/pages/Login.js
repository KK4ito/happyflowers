import React from 'react'
import { connect } from 'react-redux'
import { browserHistory } from 'react-router'
import Alert from 'react-s-alert'
import Loader from '../components/Loader'
import { login } from '../actions'
import './Login.css'

class Login extends React.Component {
  constructor(props) {
    super(props)

    this.state = {
      password: ''
    }

    this.login = this.login.bind(this)
  }

  componentDidMount() {
    if (this.props.jwt) {
      browserHistory.push('/')
    }
  }

  login(event) {
    event.preventDefault()

    let fd = new FormData()
    fd.append('password', this.state.password)

    this.props.dispatch(login(fd))
      .then(() => browserHistory.push('/'))
      .catch(() => Alert.error('The server was unable to verify your password.'))
  }

  render() {
    return (
      <main className="site">
        <form className="login">
          <Loader loading={this.props.isLoggingIn} />
          <h1 className="form-title">
            happy flowers
          </h1>
          <input className="text-input full-width spaced"
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

const mapStateToProps = state => ({
  jwt: state.auth.jwt,
  isLoggingIn: state.auth.isLoggingIn
})

export default connect(mapStateToProps)(Login)
