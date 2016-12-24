import React from 'react'
import { connect } from 'react-redux'
import Widget from '../components/Widget'
import './Stream.css'

/**
 * Class representing the live stream widget.
 */
class Stream extends React.Component {
  static propTypes = {
    name: React.PropTypes.string
  }

  constructor(props) {
    super(props)

    this.state = {
      fullscreen: false
    }
  }

  render() {
    const { name } = this.props

    return (
      <Widget title="Livestream">
        <div onClick={() => this.setState({ fullscreen: false })} className={`full-screen ${this.state.fullscreen ? 'is-visible' : ''}`}>
          <img className="live-stream" src={`http://${window.location.hostname}:8081`} alt={`Livestream of ${name || 'your flower'}.`} />
        </div>
        <img onClick={() => this.setState({ fullscreen: true })} className="live-stream" src={`http://${window.location.hostname}:8081`} alt={`Livestream of ${name || 'your flower'}.`} />
      </Widget>
    )
  }
}

const mapStateToProps = state => ({
  name: state.settings.data.get('name')
})

export default connect(
  mapStateToProps
)(Stream)
