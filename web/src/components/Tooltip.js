import React from 'react'
import './Tooltip.css'

/**
 * Functional component representing a tooltip containing further information
 * about a widget.
 *
 * @param {object} props - Standard React props, destructured to only get the
 *                         text prop.
 *
 * @return {string} - HTML markup for the component.
 */
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
