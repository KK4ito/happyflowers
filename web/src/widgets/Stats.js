import React from 'react'
import { connect } from 'react-redux'
import timeago from 'timeago.js'
import Widget from './Widget'
import './Stats.css'

// timego is used to display event dates relatively (e.g. 2 hours ago).

const ta = new timeago()

/**
 * Functional component representing the stats widget, i.e. the widget
 * containing information about the latest event dates and the option to trigger
 * the pump manually. Event dates are shown relatively if they're available,
 * otherwise a default message is shown.
 *
 * @TODO default message needs to depend on the timeframe configuration, i.e. it
 * needs to change if the configuration diverges from the default 14 days
 * period.
 *
 * @param {object} props - Standard React props, destructured to only get the
 *                         isLoggedIn, name, timestamps, isFetching props.
 *
 * @return {string} - HTML markup for the component.
 */
const Stats = ({ isLoggedIn, name, timestamps, isFetching }) => (
  <Widget title={name || 'happy flower'} isLoading={isFetching}>
    <ul className="stats-list unstyled-list">
      <li>
        <span data-icon="loupe" />
        <h3 className="stats-heading">
          Last measurement
        </h3>
        {(timestamps.measurement && ta.format(timestamps.measurement.get('measurementTimestamp'))) || 'more than two weeks ago'}
      </li>
      <li>
        <span data-icon="drop" />
        <h3 className="stats-heading">
          Last automatic watering
        </h3>
        {(timestamps.automatic && ta.format(timestamps.automatic.get('eventTimestamp'))) || 'more than two weeks ago'}
      </li>
      <li>
      <span data-icon="hand" />
        <h3 className="stats-heading">
          Last manual watering
        </h3>
        {(timestamps.manual && ta.format(timestamps.manual.get('eventTimestamp'))) || 'more than two weeks ago'}
      </li>
    </ul>
    <button data-button="block secondary" disabled={!isLoggedIn}>
      Start pump manually
    </button>
  </Widget>
)

/**
 * Map Redux state to React props for the Login component.
 *
 * @param {object} state - The Redux state, injected by the <code>connect</code>
 *                         function.
 */
const mapStateToProps = state => ({
  isLoggedIn: state.auth.jwt,
  name: state.settings.data.get('name'),
  timestamps: {
    measurement: state.history.measurements.last(),
    automatic: state.history.events.filter(e => e.get('eventType') === 'automatic').last(),
    manual: state.history.events.filter(e => e.get('eventType') === 'manual').last()
  },
  isFetching: state.settings.isFetching || state.history.isFetching
})

export default connect(mapStateToProps)(Stats)
