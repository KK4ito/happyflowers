import React from 'react'
import { connect } from 'react-redux'
import Alert from 'react-s-alert'
import Header from './Header'
import Snapshot from './Snapshot'
import Stats from './Stats'
import History from './History'
import Stream from './Stream'
import { fetchSettings, connectWS, disconnectWS } from '../actions'

/**
 * Class representing the main dashboard of the application. It contains all
 * relevant data the application offers.
 *
 * @extends React.Component
 */
class Dashboard extends React.Component {
  static propTypes = {
    dispatch: React.PropTypes.func.isRequired
  }

  /**
   * Lifecycle method that is executed whenever the component is mounted.
   * Dispatches Redux actions to load required data.
   */
  componentDidMount() {
    const { dispatch } = this.props

    dispatch(fetchSettings())
      .catch(() => Alert.error('Could not retrieve settings.'))

    dispatch(connectWS())
      .onclose = e => e.code === 1006 && Alert.error('Could not connect to the WebSockets server.')
  }

  /**
   * Lifecycle method that is executed whenever the component is unmounted.
   * Attempts to disconnect the user from the WebSockets server.
   */
  componentWillUnmount() {
    this.props.dispatch(disconnectWS())
  }

  /**
   * Lifecycle method that is executed whenever the component is to receive new
   * props unmounted. Clears open timeouts if they are available and the flower
   * is no longer busy.
   *
   * @param {object} props - Standard React props.
   */
  componentWillReceiveProps({ auth }) {
    if (this.props.auth !== auth && auth === '') {
      Alert.success('Successfully logged out.')
    }
  }

  /**
   * Renders the component.
   *
   * @return {string} - HTML markup for the component.
   */
  render() {
    return (
      <main className="site">
        <Header />
        <div className="wrap">
          <div data-grid>
            <div data-col="L1-2">
              <Snapshot />
            </div>
            <div data-col="L1-2">
              <Stats />
            </div>
          </div>
          <History />
          <Stream />
        </div>
        <Alert stack={{limit: 3}}
               timeout={2000}
               effect="slide"
               position="bottom" />
      </main>
    )
  }
}

/**
 * Map Redux state to React props for the Header component.
 *
 * @param {object} state - The Redux state, injected by the <code>connect</code>
 *                         function.
 */
const mapStateToProps = state => ({
  auth: state.auth.jwt
})

export default connect(mapStateToProps)(Dashboard)
