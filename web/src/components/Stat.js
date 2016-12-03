import React from 'react'
import TimeAgo from 'timeago-react'
import './Stat.css'

/**
 * Functional component representing an entry of the stats list.
 *
 * @param {object} props - Standard React props.
 *
 * @return {string} - HTML markup for the component.
 */
const Stat = ({ icon, title, color, timestamp }) => (
  <li className="stats-item">
    <span data-icon={icon} />
    <h3 className="stats-heading">
      {title}
    </h3>
    {color && <span className="circle" style={{ backgroundColor: color }}></span>}
    {(timestamp && <TimeAgo datetime={timestamp} />) || 'a while ago'}
  </li>
)

export default Stat
