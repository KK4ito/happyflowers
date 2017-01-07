import React from 'react'
import { connect } from 'react-redux'
import { Link } from 'react-router'
import Widget from '../components/Widget'
import Stat from '../components/Stat'
import { triggerPump } from '../actions'
import { eventKinds } from '../strings'
import './Stats.css'

/**
 * Class representing the stats widget, i.e. the widget containing information
 * about the latest event dates and the option to trigger the pump manually.
 * Event dates are shown relatively if they're available, otherwise a default
 * message is shown.
 */
class Stats extends React.Component {
  static propTypes = {
    dispatch: React.PropTypes.func.isRequired,
    isLoggedIn: React.PropTypes.bool,
    busy: React.PropTypes.bool,
    name: React.PropTypes.string,
    timestamps: React.PropTypes.shape({
      measurement: React.PropTypes.object,
      automatic: React.PropTypes.object,
      manual: React.PropTypes.object
    }),
    isFetching: React.PropTypes.bool,
    socket: React.PropTypes.object
  }

  constructor(props) {
    super(props)

    this.state = {
      pump: 0
    }

    this.handleTrigger = this.handleTrigger.bind(this)
  }

  componentWillReceiveProps({ busy }) {
    if (!busy && this.timeouts) {
      this.timeouts.forEach(t => clearTimeout(t))
      this.setState({ pump: 0 })
    }
  }

  /**
   * Handles clicks on the manual trigger button. Triggers the relevant action
   * and updates the state of the button.
   */
  handleTrigger() {
    this.props.dispatch(triggerPump(this.props.socket))
    this.setState({ pump: 1 })

    this.timeouts = [
      setTimeout(() => this.setState({ pump: 2 }), 1000),
      setTimeout(() => this.setState({ pump: 3 }), 4000),
      setTimeout(() => {
        this.setState({ pump: 0 })
        this.timeouts = null
      }, 7000)
    ]
  }

  render() {
    const { isLoggedIn, name, timestamps, isFetching, busy, socket } = this.props
    const { pump } = this.state

    return (
      <Widget title={name || 'Your Flower'}
              isLoading={isFetching}>
        <ul className="stats-list unstyled-list">
          <Stat icon="loupe"
                title="Last Measurement"
                timestamp={(timestamps.measurement && timestamps.measurement.get('measurementTimestamp')) || ''} />
          <Stat icon="drop"
                title="Last Automatic Watering"
                timestamp={(timestamps.automatic && timestamps.automatic.get('eventTimestamp')) || ''}
                color="rgba(0, 0, 255, 0.5)" />
          <Stat icon="hand"
                title="Last Manual Watering"
                timestamp={(timestamps.manual && timestamps.manual.get('eventTimestamp')) || ''}
                color="rgba(255, 0, 0, 0.5)" />
        </ul>
        {!isLoggedIn &&
          <Link data-button="block secondary"
                disabled={busy}
                to="/login">
            {busy ? `${name} is busy…` : 'Login to start pump'}
          </Link>
        }
        {isLoggedIn &&
          <button data-button="block secondary"
                  disabled={busy || !socket || socket.readyState !== 1}
                  onClick={this.handleTrigger}>
            {busy && pump === 0
              ? `${name} is busy…`
              : pump === 0
                ? 'Start pump manually'
                : pump === 1
                  ? `Checking ${name || 'your flower'}…`
                  : pump === 2
                    ? 'Watering…'
                    : 'Seeping in…'}
          </button>
        }
      </Widget>
    )
  }
}

const mapStateToProps = state => ({
  isLoggedIn: !!state.auth.jwt,
  busy: state.settings.busy,
  name: state.settings.data.get('name'),
  timestamps: {
    measurement: state.history.measurements.last(),
    automatic: state.history.events.filter(e => e.get('eventKind') === eventKinds.automatic).last(),
    manual: state.history.events.filter(e => e.get('eventKind') === eventKinds.manual).last()
  },
  isFetching: state.settings.isFetching || state.history.isFetching
})

export default connect(mapStateToProps)(Stats)
