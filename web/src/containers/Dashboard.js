import React from 'react'
import { connect } from 'react-redux'
import Header from './Header'
import Snapshot from './Snapshot'
import Stats from './Stats'
import History from './History'
import Stream from './Stream'
import { fetchSettings, fetchHistory, connectWS, disconnectWS } from '../actions'

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
    dispatch(fetchHistory())
    dispatch(connectWS())
  }

  /**
   * Lifecycle method that is executed whenever the component is unmounted.
   * Attempts to disconnect the user from the WebSockets server.
   */
  componentWillUnmount() {
    this.props.dispatch(disconnectWS())
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
      </main>
    )
  }
}

export default connect()(Dashboard)
