import React from 'react'
import Petals from './Petals'
import './Flower.css'

/**
 * Functional component representing the current moisture level of the plant.
 * The component renders SVG code.
 *
 * @param {object} props - Standard React props.
 *
 * @return {string} - HTML markup for the component.
 */
const Flower = ({ value, updating }) => (
  <svg className="flower"
       width="285"
       height="285"
       viewBox="0 0 285 285"
       xmlns="http://www.w3.org/2000/svg">
    <g fill="none"
       fillRule="evenodd">
      <g transform="translate(15 15)"
         strokeWidth="30"
         strokeLinecap="round">
        <circle stroke="#CFCFCF"
                strokeDasharray="801,801"
                cx="127.5"
                cy="127.5"
                r="127.5" />
        <path className="progress"
              d="M0 127.5C0 57.084 57.084 0 127.5 0S255 57.084 255 127.5 197.916 255 127.5 255 0 197.916 0 127.5"
              stroke="#4CADEB"
              strokeDasharray={`${839 * value / 100},839`} />
      </g>
      <Petals updating={updating} />
    </g>
  </svg>
)

Flower.propTypes = {
  value: React.PropTypes.number.isRequired,
  updating: React.PropTypes.bool
}

export default Flower
