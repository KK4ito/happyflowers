import React from 'react'
import { connect } from 'react-redux'
import Widget from './Widget'
import Flower from '../components/Flower'
import './Snapshot.css'

const Snapshot = ({ snapshot, isFetching }) => (
  <Widget title="Snapshot" isLoading={isFetching}>
    <Flower value={snapshot} />
  </Widget>
)

const mapStateToProps = state => ({
  snapshot: state.history.snapshot,
  isFetching: state.history.isFetching
})

export default connect(mapStateToProps)(Snapshot)
