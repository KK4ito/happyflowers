import React from 'react'
import { connect } from 'react-redux'
import Header from '../components/Header'
import Snapshot from '../widgets/Snapshot'
import Stats from '../widgets/Stats'
import History from '../widgets/History'
import Stream from '../widgets/Stream'
import { fetchSettings, fetchHistory } from '../actions'

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
