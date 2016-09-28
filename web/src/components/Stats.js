import React from 'react'

const Stats = () => (
  <section className="stats">
    <ul className="stats-list unstyled-list">
      <li>Last automatic watering: –––</li>
      <li>Last manual watering: –––</li>
    </ul>
    <button data-button="block">Start Watering</button>
    <button data-button="block">Show History</button>
  </section>
)

export default Stats
