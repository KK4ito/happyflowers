import React from 'react'
import './Loader.css'

const Loader = ({ loading }) => (
  <div className={`loader-container ${loading ? 'is-loading' : ''}`}>
    <div className="loader">
      <div></div>
      <div></div>
      <div></div>
    </div>
  </div>
)

export default Loader
