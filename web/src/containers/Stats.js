import React from 'react'
import { connect } from 'react-redux'
import { Link } from 'react-router'
import Widget from '../components/Widget'
import Stat from '../components/Stat'
import { triggerPump } from '../actions'
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
    isFetching: React.PropTypes.bool
  }

  /**
   * Create a Stats component. Sets initial state and binds class methods.
   *
   * @param {object} props - Standard react props to be passed to the parent
   *                         constructor.
   */
  constructor(props) {
    super(props)

    this.state = {
      pump: 0
    }

    this.handleTrigger = this.handleTrigger.bind(this)
  }

  /**
   * Lifecycle method that is executed whenever the component is to receive new
   * props unmounted. Clears open timeouts if they are available and the flower
   * is no longer busy.
   */
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
    this.props.dispatch(triggerPump())
    this.setState({ pump: 1 })

    this.timeouts = [
      setTimeout(() => this.setState({ pump: 2 }), 3000),
      setTimeout(() => this.setState({ pump: 3 }), 8000),
      setTimeout(() => {
        this.setState({ pump: 0 })
        this.timeouts = null
      }, 16000)
    ]
  }

  /**
   * Renders the component.
   *
   * @return {string} - HTML markup for the component.
   */
  render() {
    const { isLoggedIn, name, timestamps, isFetching, busy } = this.props
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
                to="/login">
            Login to start pump
          </Link>
        }
        {isLoggedIn &&
          <button data-button="block secondary"
                  disabled={busy}
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
