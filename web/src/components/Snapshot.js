import React from 'react'
import { connect } from 'react-redux'
import Flower from './Flower'
import Loader from './Loader'
import './Snapshot.css'

const Snapshot = ({ snapshot, isFetching }) => (
  <section className="snapshot widget spaced">
    <Loader loading={isFetching} />
    <h2 className="widget-title">
      Snapshot
    </h2>
    <div className="widget-body">
      <Flower value={snapshot} />
    </div>
  </section>
)

const mapStateToProps = (state) => ({
  snapshot: state.history.snapshot,
  isFetching: state.history.isFetching
})

export default connect(mapStateToProps)(Snapshot)
