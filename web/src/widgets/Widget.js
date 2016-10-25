import React from 'react'
import Loader from '../components/Loader'
import Tooltip from '../components/Tooltip'
import './Widget.css'

/**
 * Functional component representing a general-purpose widget, containing a
 * title, optional loading indicators and tooltips, and a body container.
 *
 * @param {object} props - Standard React props, destructured to only get the
 *                         title, tooltip, isLoading, children props.
 *
 * @return {string} - HTML markup for the component.
 */
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

Widget.propTypes = {
  title: React.PropTypes.string.isRequired,
  tooltip: React.PropTypes.string,
  isLoading: React.PropTypes.bool,
  children: React.PropTypes.node
}

export default Widget
