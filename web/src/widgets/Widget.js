import React from 'react'
import Loader from '../components/Loader'
import Tooltip from '../components/Tooltip'
import './Widget.css'

const Widget = ({ title, tooltip, isLoading, children }) => (
  <section className="stats widget spaced">
    {isLoading !== undefined && <Loader loading={isLoading} />}
    <h2 className="widget-title">
      {title}
      {tooltip && <Tooltip text={tooltip} />}
    </h2>
    <div className="widget-body">
      {children}
    </div>
  </section>
)

export default Widget
