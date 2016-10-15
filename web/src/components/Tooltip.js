import React from 'react'
import './Tooltip.css'

const Tooltip = ({ text }) => (
  <div className="tooltip">
    <span data-icon="help">
      <span className="tooltip-body">
        {text}
      </span>
    </span>
  </div>
)

export default Tooltip
