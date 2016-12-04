import React from 'react'
import { connect } from 'react-redux'
import Widget from '../components/Widget'
import Flower from '../components/Flower'
import './Snapshot.css'

/**
 * Functional component representing the snapshot widget, i.e. the widget
 * containing information about the current moisture level of the flower.
 *
 * @param {object} props - Standard React props.
 *
 * @return {string} - HTML markup for the component.
 */
const Snapshot = ({ snapshot, isFetching, busy }) => (
  <Widget title="Snapshot"
          isLoading={isFetching}>
    <Flower value={snapshot} updating={busy} />
  </Widget>
)

Snapshot.propTypes = {
  snapshot: React.PropTypes.number.isRequired,
  busy: React.PropTypes.bool,
  isFetching: React.PropTypes.bool
}

/**
 * Map Redux state to React props for the Login component.
 *
 * @param {object} state - The Redux state, injected by the <code>connect</code>
 *                         function.
 */
const mapStateToProps = state => ({
  snapshot: state.history.snapshot,
  busy: state.settings.busy,
  isFetching: state.history.isFetching
})

export default connect(mapStateToProps)(Snapshot)
