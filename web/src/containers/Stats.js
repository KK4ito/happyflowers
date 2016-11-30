import React from 'react'
import { connect } from 'react-redux'
import TimeAgo from 'timeago-react'
import Widget from '../components/Widget'
import { triggerPump } from '../actions'
import './Stats.css'

/**
 * Class representing the stats widget, i.e. the widget containing information
 * about the latest event dates and the option to trigger the pump manually.
 * Event dates are shown relatively if they're available, otherwise a default
 * message is shown.
 */
class Stats extends React.Component {
  /** TODO: document */
  static propTypes = {
    dispatch: React.PropTypes.function,
    isLoggedIn: React.PropTypes.bool,
    busy: React.PropTypes.bool,
    name: React.PropTypes.string,
    timestamps: React.PropTypes.shape({
      measurement: React.PropTypes.object,
      automatic: React.PropTypes.object,
      manual: React.PropTypes.object
    }),
    isFetching: React.PropTypes.bool
  }

  /** TODO: document */
  constructor(props) {
    super(props)

    this.state = {
      pump: 0
    }

    this.handleTrigger = this.handleTrigger.bind(this)
  }

  /** TODO: document */
  handleTrigger() {
    this.props.dispatch(triggerPump())
    this.setState({ pump: 1 })

    setTimeout(() => this.setState({ pump: 2 }), 3000)
    setTimeout(() => this.setState({ pump: 3 }), 8000)
    setTimeout(() => this.setState({ pump: 0 }), 16000)
  }

  render() {
    const { isLoggedIn, name, timestamps, isFetching, busy } = this.props
    const { pump } = this.state

    return (
      <Widget title={name || 'Your Flower'} isLoading={isFetching}>
        <ul className="stats-list unstyled-list">
          <li>
            <span data-icon="loupe" />
            <h3 className="stats-heading">
              Last measurement
            </h3>
            {(timestamps.measurement && <TimeAgo datetime={timestamps.measurement.get('measurementTimestamp')} />) || 'a while ago'}
          </li>
          <li>
            <span data-icon="drop" />
            <h3 className="stats-heading">
              Last automatic watering
            </h3>
            <span className="circle circle-automatic"></span>{(timestamps.automatic && <TimeAgo datetime={timestamps.automatic.get('eventTimestamp')} />) || 'a while ago'}
          </li>
          <li>
          <span data-icon="hand" />
            <h3 className="stats-heading">
              Last manual watering
            </h3>
            <span className="circle circle-manual"></span>{(timestamps.manual && <TimeAgo datetime={timestamps.manual.get('eventTimestamp')} />) || 'a while ago'}
          </li>
        </ul>
        <button data-button="block secondary" disabled={!isLoggedIn || busy} onClick={this.handleTrigger}>
          {busy && pump === 0 ? `${name} is busy…` : pump === 0 ? 'Start pump manually' : pump === 1 ? `Checking ${name || 'your flower'}…` : pump === 2 ? 'Watering…' : 'Seeping in…'}
        </button>
      </Widget>
    )
  }
}

/**
 * Map Redux state to React props for the Login component.
 *
 * @param {object} state - The Redux state, injected by the <code>connect</code>
 *                         function.
 */
const mapStateToProps = state => ({
  isLoggedIn: !!state.auth.jwt,
  busy: state.settings.busy,
  name: state.settings.data.get('name'),
  timestamps: {
    measurement: state.history.measurements.last(),
    automatic: state.history.events.filter(e => e.get('eventType') === 'automatic').last(),
    manual: state.history.events.filter(e => e.get('eventType') === 'manual').last()
  },
  isFetching: state.settings.isFetching || state.history.isFetching
})

export default connect(mapStateToProps)(Stats)
