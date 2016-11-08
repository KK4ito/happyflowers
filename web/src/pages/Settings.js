import React from 'react'
import IPropTypes from 'react-immutable-proptypes'
import { connect } from 'react-redux'
import { browserHistory } from 'react-router'
import { Map } from 'immutable'
import Alert from 'react-s-alert'
import Header from '../components/Header'
import Loader from '../components/Loader'
import { fetchSettings, submitSettings, connectWS, disconnectWS } from '../actions'
import '../components/Alert.css'

/**
 * Class representing the settings screen of the application.
 *
 * @extends React.Component
 */
class Settings extends React.Component {
  static propTypes = {
    dispatch: React.PropTypes.func.isRequired,
    jwt: React.PropTypes.string,
    settings: IPropTypes.map.isRequired,
    isFetching: React.PropTypes.bool,
    isSubmitting: React.PropTypes.bool
  }

  /**
   * Create a Settings component. Sets initial state and binds class methods.
   *
   * @param {object} props - Standard react props to be passed to the parent
   *                         constructor.
   */
  constructor(props) {
    super(props)

    // The state contains values and validity for all settings fields. They all
    // consist of an immutable Map.

    this.state = {
      error: false,
      pristine: true,
      name: Map({
        value: '',
        valid: false
      }),
      upper: Map({
        value: 0,
        valid: false
      }),
      lower: Map({
        value: 0,
        valid: false
      }),
      interval: Map({
        value: 0,
        valid: false
      })
    }

    this.submitForm = this.submitForm.bind(this)
    this.handleTextChange = this.handleTextChange.bind(this)
  }

  /**
   * Lifecycle method that is executed whenever the component is mounted.
   * Redirects the user if they're not logged in, otherwise attempts to retrieve
   * settings.
   */
  componentDidMount() {
    const { dispatch, jwt } = this.props

    if (!jwt) {
      browserHistory.push('/login')
    }

    // Show an error message if fetching the application settings was not
    // successful.

    dispatch(fetchSettings())
      .catch(err => Alert.error('Could not retrieve settings.'))

    dispatch(connectWS())
  }

  /**
   * @TODO document
   */
  componentWillUnmount() {
    this.props.dispatch(disconnectWS())
  }

  /**
   *
   * @param {object} props - Standard React props, destructured to only get the
   *                         immutable settings Map.
   */
  componentWillReceiveProps({ settings }) {

    // Don't change the state if there are no settings to display.

    if (settings.isEmpty() || this.state.error) {
      this.setState({ error: false })
      return
    }

    // The new state is built based on the settings Map, passed through the
    // props. All values are considered to be valid and the form is considered
    // pristine.

    this.setState({
      pristine: true,
      name: Map({
        value: settings.get('name'),
        valid: true
      }),
      upper: Map({
        value: settings.get('upper'),
        valid: true
      }),
      lower: Map({
        value: settings.get('lower'),
        valid: true
      }),
      interval: Map({
        value: settings.get('interval'),
        valid: true
      })
    })
  }

  /**
   * Attempt to save the edited settings.
   *
   * @param {object} event - Submit event from the settings form. Used to
   *                         prevent the default behaviour in favour of AJAX
   *                         functionality.
   */
  submitForm(event) {
    event.preventDefault()

    // Exit early if the form is still pristine or any of the required fields
    // are invalid.

    if (this.state.pristine || !this.state.name.get('valid') || !this.state.upper.get('valid') || !this.state.lower.get('valid') || !this.state.interval.get('valid')) {
      return
    }

    // Create a FormData object used to submit all required data along with the
    // request.

    const data = {
      name: this.state.name.get('value'),
      upper: `${this.state.upper.get('value')}`,
      lower: `${this.state.lower.get('value')}`,
      interval: `${this.state.interval.get('value')}`,
      token: this.props.jwt
    }

    // Inform the user about the response, showing confirmation or error if the
    // request was successful or erroneous, respectively.

    this.props.dispatch(submitSettings(data))
      .then(() => Alert.success('Settings saved successfully.'))
      .catch(() => {
        this.setState({ error: true })
        Alert.error('Could not save settings.')
      })
  }

  /**
   * Handle change in the value of any input fields.
   *
   * @param {string} key - The key of the field for which the state should be
   *                       updated.
   * @param {string} value - The new value that is to be stored for the field.
   * @param {boolean} valid - A boolean that determines whether the value of the
   *                          field is valid. It is up to the caller to
   *                          determine the logic behind the validation.
   */
  handleTextChange(key, value, valid) {
    this.setState({
      pristine: false,
      [key]: Map({ value, valid })
    })
  }

  /**
   * Renders the component.
   *
   * @return {string} - HTML markup for the component.
   */
  render() {
    const { name, upper, lower, interval } = this.state

    return (
      <main className="site">
        <Header />
        <div className="wrap">
          <section className="widget spaced">
            <Loader loading={this.props.isFetching} />
            <h2 className="widget-title">
              Settings
            </h2>
            <div className="widget-body">
              <form className="settings">
                <div data-grid>
                  <div data-col="L1-4">
                    <label htmlFor="name"
                           className="spaced">
                      Name
                    </label>
                  </div>
                  <div data-col="L3-4">
                    <input id="name"
                           type="text"
                           className={`text-input full-width spaced ${!name.get('valid') ? 'is-invalid' : ''}`}
                           value={name.get('value')}
                           onChange={ev => this.handleTextChange('name', ev.target.value, ev.target.value.length > 0)} />
                  </div>
                  <hr className="separator" />
                  <div data-col="L1-4">
                    <label htmlFor="upper"
                           className="spaced">
                      Upper limit
                    </label>
                  </div>
                  <div data-col="L3-4">
                    <input id="upper"
                           type="number"
                           className={`text-input full-width spaced ${!upper.get('valid') ? 'is-invalid' : ''}`}
                           value={upper.get('value')}
                           min="0"
                           max="100"
                           onChange={ev => this.handleTextChange('upper', +ev.target.value, +ev.target.value % 1 === 0 && +ev.target.value >= 0 && +ev.target.value <= 100 && +ev.target.value > lower.get('value'))} />
                    <p>
                      The upper limit determines at which percentage of soil moisture watering should be disabled. Manual watering automatically stops at this level if it is not manually stopped. A number between 0 and 100 is expected. Make sure the number is greater than the lower level.
                    </p>
                  </div>
                  <div data-col="L1-4">
                    <label htmlFor="lower"
                           className="spaced">
                      Lower limit
                    </label>
                  </div>
                  <div data-col="L3-4">
                    <input id="lower"
                           type="number"
                           className={`text-input full-width spaced ${!lower.get('valid') ? 'is-invalid' : ''}`}
                           value={lower.get('value')}
                           min="0"
                           max="100"
                           onChange={ev => this.handleTextChange('lower', +ev.target.value, +ev.target.value % 1 === 0 && +ev.target.value >= 0 && +ev.target.value <= 100 && +ev.target.value < upper.get('value'))} />
                    <p>
                      The lower limit determines over which percentage of soil moisture the plant should always be kept. Automatic watering will always attempt to keep the moisture above this level. A number between 0 and 100 is expected. Make sure the number is smaller than the upper level.
                    </p>
                  </div>
                  <hr className="separator" />
                  <div data-col="L1-4">
                    <label htmlFor="interval"
                           className="spaced">
                      Measurement Interval
                    </label>
                  </div>
                  <div data-col="L3-4">
                    <input id="interval"
                           type="number"
                           className={`text-input full-width spaced ${!interval.get('valid') ? 'is-invalid' : ''}`}
                           value={interval.get('value')}
                           min="0"
                           onChange={ev => this.handleTextChange('interval', +ev.target.value, +ev.target.value % 1 === 0 && +ev.target.value > 0)} />
                    <p>
                      The measurement interval determines the regularity at which the soil moisture percentage is measured. An interval of 60 minutes or more is recommended to ensure optimal performance. A number greater than 0 is expected.
                    </p>
                  </div>
                </div>
                <input type="submit"
                       data-button="block"
                       disabled={!name.get('valid') || !upper.get('valid') || !lower.get('valid') || !interval.get('valid') || this.state.pristine || this.props.isSubmitting}
                       value={this.props.isSubmitting ? 'Saving…' : 'Save settings'}
                       onClick={this.submitForm} />
              </form>
            </div>
          </section>
        </div>
        <Alert stack={{limit: 3}}
               timeout={2000}
               effect="slide"
               position="bottom" />
      </main>
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
  jwt: state.auth.jwt,
  settings: state.settings.data,
  isFetching: state.settings.isFetching,
  isSubmitting: state.settings.isSubmitting
})

export default connect(mapStateToProps)(Settings)
