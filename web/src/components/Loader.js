import React from 'react'
import './Loader.css'

/**
 * Functional component representing a loading indicator. It can be shown or
 * hidden using the loading prop.
 *
 * @param {object} props - Standard React props, destructured to only get the
 *                         loading prop.
 *
 * @return {string} - HTML markup for the component.
 */
const Loader = ({ loading }) => (
  <div className={`loader-container ${loading ? 'is-loading' : ''}`}>
    <div className="loader">
      <div></div>
      <div></div>
      <div></div>
    </div>
  </div>
)

Loader.propTypes = {
  loading: React.PropTypes.bool
}

export default Loader
