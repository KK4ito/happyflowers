import React from 'react'
import { connect } from 'react-redux'
import Loader from './Loader'
import './Stats.css'

const Stats = ({ name, isFetching }) => (
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
            Last checked
          </h3>
          <span title="30. Sep. 2016 — 17:37">
            30 seconds ago
          </span>
        </li>
        <li>
          <span data-icon="drop" />
          <h3 className="stats-heading">
            Last automatic watering
          </h3>
          <span title="30. Sep. 2016 — 17:35">
            2 minutes ago
          </span>
        </li>
        <li>
        <span data-icon="hand" />
          <h3 className="stats-heading">
            Last manual watering
          </h3>
          <span title="30. Sep. 2016 — 17:07">
            30 minutes ago
          </span>
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
  isFetching: state.settings.isFetching || state.history.isFetching
})

export default connect(
  mapStateToProps
)(Stats)
