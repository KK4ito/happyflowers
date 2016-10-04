import React from 'react'
import Flower from './Flower'
import './Snapshot.css'

const Snapshot = () => (
  <section className="snapshot widget spaced">
    <h2 className="widget-title">
      Snapshot
    </h2>
    <div className="widget-body">
      <Flower />
    </div>
  </section>
)

export default Snapshot
