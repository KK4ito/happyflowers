import React from 'react'
import { connect } from 'react-redux'
import Widget from './Widget'
import Flower from '../components/Flower'
import './Snapshot.css'

/**
 * Functional component representing the snapshot widget, i.e. the widget
 * containing information about the current moisture level of the flower.
 *
 * @param {object} props - Standard React props, destructured to only get the
 *                         snapshot and isFetching props.
 *
 * @return {string} - HTML markup for the component.
 */
const Snapshot = ({ snapshot, isFetching }) => (
  <Widget title="Snapshot" isLoading={isFetching}>
    <Flower value={snapshot} />
  </Widget>
)

/**
 * Map Redux state to React props for the Login component.
 *
 * @param {object} state - The Redux state, injected by the <code>connect</code>
 *                         function.
 */
const mapStateToProps = state => ({
  snapshot: state.history.snapshot,
  isFetching: state.history.isFetching
})

export default connect(mapStateToProps)(Snapshot)
