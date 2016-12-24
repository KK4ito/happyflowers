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
const Flower = ({ moisture, temperature, updating }) => (
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
              stroke={moisture >= 0.666 ? '#f0d24e' : moisture >= 0.333 ? '#4cadeb' : '#eb4c4c'}
              strokeDasharray={`${Math.round(839 * moisture)},839`} />
      </g>
      <Petals updating={updating} />
    </g>
    <g className={`flower-temp ${updating ? 'is-updating' : ''}`}>
      <text x="142.5"
            y="142.5"
            textAnchor="middle"
            alignmentBaseline="central">
        {`${temperature} °C`}
      </text>
    </g>
  </svg>
)

Flower.propTypes = {
  moisture: React.PropTypes.number.isRequired,
  temperature: React.PropTypes.number.isRequired,
  updating: React.PropTypes.bool
}

export default Flower
