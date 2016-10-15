import React from 'react'
import { connect } from 'react-redux'
import timeago from 'timeago.js'
import Loader from './Loader'
import './Stats.css'

const ta = new timeago()

const Stats = ({ name, timestamps, isFetching }) => (
  <section className="stats widget spaced">
    <Loader loading={isFetching} />
    <h2 className="widget-title">
      {name || 'happy flower'}
    </h2>
    <div className="widget-body">
      <ul className="stats-list unstyled-list">
        <li>
          <span data-icon="loupe" />
          <h3 className="stats-heading">
            Last measurement
          </h3>
          {(timestamps.measurement && ta.format(timestamps.measurement)) || 'more than two weeks ago'}
        </li>
        <li>
          <span data-icon="drop" />
          <h3 className="stats-heading">
            Last automatic watering
          </h3>
          {(timestamps.automatic && ta.format(timestamps.automatic)) || 'more than two weeks ago'}
        </li>
        <li>
        <span data-icon="hand" />
          <h3 className="stats-heading">
            Last manual watering
          </h3>
          {(timestamps.manual && ta.format(timestamps.manual)) || 'more than two weeks ago'}
        </li>
      </ul>
      <button data-button="block secondary">
        Start pump manually
      </button>
    </div>
  </section>
)

const mapStateToProps = (state) => ({
  name: state.settings.data.name,
  timestamps: {
    measurement: state.history.measurements.map(m => m.measurementTimestamp).slice(-1).pop(),
    automatic: state.history.events.filter(e => e.eventType === 'automatic').map(e => e.eventTimestamp).slice(-1).pop(),
    manual: state.history.events.filter(e => e.eventType === 'manual').map(e => e.eventTimestamp).slice(-1).pop()
  },
  isFetching: state.settings.isFetching || state.history.isFetching
})

export default connect(mapStateToProps)(Stats)
