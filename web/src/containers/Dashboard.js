import React from 'react'
import { connect } from 'react-redux'
import Header from './Header'
import Snapshot from './Snapshot'
import Stats from './Stats'
import History from './History'
import Stream from './Stream'
import { fetchSettings } from '../actions'

/**
 * Class representing the main dashboard of the application. It contains all
 * relevant data the application offers.
 *
 * @extends React.Component
 */
class Dashboard extends React.Component {
  static propTypes = {
    socket: React.PropTypes.object
  }

  componentDidMount() {
    const { dispatch } = this.props
    dispatch(fetchSettings())
  }

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
              <Stats socket={this.props.socket} />
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
