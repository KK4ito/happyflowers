import React from 'react'
import './Stats.css'

const Stats = () => (
  <section className="stats widget spaced">
    <h2 className="widget-title">Cecelia</h2>
    <div className="widget-body">
      <ul className="stats-list unstyled-list">
        <li>
          <h3 className="stats-heading">Last checked</h3>
          <span className="muted">30 seconds ago</span>
        </li>
        <li>
          <h3 className="stats-heading">Last automatic watering</h3>
          <span className="muted">2 minutes ago</span>
        </li>
        <li>
          <h3 className="stats-heading">Last manual watering</h3>
          <span className="muted">30 minutes ago</span>
        </li>
      </ul>
      <button data-button="block secondary">Start pump manually</button>
    </div>
  </section>
)

export default Stats
