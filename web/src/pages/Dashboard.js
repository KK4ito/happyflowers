import React from 'react'
import { connect } from 'react-redux'
import Header from '../components/Header'
import Snapshot from '../components/Snapshot'
import Stats from '../components/Stats'
import History from '../components/History'
import Stream from '../components/Stream'
import { fetchSettings, fetchHistory } from '../actions'

class Dashboard extends React.Component {
  componentDidMount() {
    this.props.dispatch(fetchSettings())
    this.props.dispatch(fetchHistory())
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
