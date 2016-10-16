import React from 'react'
import { connect } from 'react-redux'
import timeago from 'timeago.js'
import Widget from './Widget'
import './Stats.css'

const ta = new timeago()

const Stats = ({ name, timestamps, isFetching }) => (
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
    <button data-button="block secondary">
      Start pump manually
    </button>
  </Widget>
)

const mapStateToProps = state => ({
  name: state.settings.data.get('name'),
  timestamps: {
    measurement: state.history.measurements.last(),
    automatic: state.history.events.filter(e => e.get('eventType') === 'automatic').last(),
    manual: state.history.events.filter(e => e.get('eventType') === 'manual').last()
  },
  isFetching: state.settings.isFetching || state.history.isFetching
})

export default connect(mapStateToProps)(Stats)
