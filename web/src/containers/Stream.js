import React from 'react'
import { connect } from 'react-redux'
import Widget from '../components/Widget'
import './Stream.css'

/**
 * Functional component representing the live stream widget, i.e. the widget
 * containing a live stream of the flower.
 *
 * @return {string} - HTML markup for the component.
 */
const Stream = ({ name }) => (
  <Widget title="Livestream">
    <img className="live-stream" src={`http://${window.location.hostname}:8081`} alt={`Livestream of ${name}.`} />
  </Widget>
)

const mapStateToProps = state => ({
  name: state.settings.data.get('name')
})

export default connect(
  mapStateToProps
)(Stream)
