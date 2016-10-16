import React from 'react'
import { connect } from 'react-redux'
import Header from '../components/Header'
import Snapshot from '../widgets/Snapshot'
import Stats from '../widgets/Stats'
import History from '../widgets/History'
import Stream from '../widgets/Stream'
import { fetchSettings, fetchHistory } from '../actions'

class Dashboard extends React.Component {
  componentDidMount() {
    const { dispatch } = this.props

    dispatch(fetchSettings())
    dispatch(fetchHistory())
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
