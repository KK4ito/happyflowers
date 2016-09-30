import React from 'react'
import './Stats.css'

const Stats = () => (
  <section className="stats widget spaced">
    <h2 className="widget-title">Cecelia</h2>
    <div className="widget-body">
      <ul className="stats-list unstyled-list">
        <li>
          <span data-icon="loupe"></span>
          <h3 className="stats-heading">Last checked</h3>
          <span title="30. Sep. 2016 — 17:37">30 seconds ago</span>
        </li>
        <li>
        <span data-icon="drop"></span>
          <h3 className="stats-heading">Last automatic watering</h3>
          <span title="30. Sep. 2016 — 17:35">2 minutes ago</span>
        </li>
        <li>
        <span data-icon="hand"></span>
          <h3 className="stats-heading">Last manual watering</h3>
          <span title="30. Sep. 2016 — 17:07">30 minutes ago</span>
        </li>
      </ul>
      <button data-button="block secondary">Start pump manually</button>
    </div>
  </section>
)

export default Stats
