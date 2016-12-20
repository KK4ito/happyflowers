import React from 'react'
import { connect } from 'react-redux'
import IPropTypes from 'react-immutable-proptypes'
import Widget from '../components/Widget'
import Flower from '../components/Flower'
import { measurementKinds } from '../strings'
import './Snapshot.css'

/**
 * Functional component representing the snapshot widget, i.e. the widget
 * containing information about the current moisture level of the flower.
 *
 * @param {object} props - Standard React props.
 *
 * @return {string} - HTML markup for the component.
 */
const Snapshot = ({ snapshot, settings, isFetching, busy }) => (
  <Widget title="Snapshot"
          isLoading={isFetching}>
    <Flower moisture={snapshot.get(measurementKinds.moisture) && settings.get('lower') && settings.get('upper')
                      ? (snapshot.get(measurementKinds.moisture) - Math.max((settings.get('lower') - 20), 0)) / Math.min((settings.get('upper') + 20), 100)
                      : 0}
            temperature={snapshot.get(measurementKinds.temperature)}
            updating={busy} />
  </Widget>
)

Snapshot.propTypes = {
  snapshot: IPropTypes.map.isRequired,
  settings: IPropTypes.map.isRequired,
  busy: React.PropTypes.bool,
  isFetching: React.PropTypes.bool
}

const mapStateToProps = state => ({
  snapshot: state.history.snapshot,
  settings: state.settings.data,
  busy: state.settings.busy,
  isFetching: state.history.isFetching
})

export default connect(mapStateToProps)(Snapshot)
