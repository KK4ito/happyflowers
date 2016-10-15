import React from 'react'
import { connect } from 'react-redux'
import Alert from 'react-s-alert'
import Header from '../components/Header'
import Loader from '../components/Loader'
import { fetchSettings, submitSettings } from '../actions'
import '../components/Alert.css'

class Settings extends React.Component {
  constructor(props) {
    super(props)

    this.state = {
      pristine: true,
      name: {
        value: "",
        valid: false
      },
      upper: {
        value: 0,
        valid: false
      },
      lower: {
        value: 0,
        valid: false
      },
      interval: {
        value: 0,
        valid: false
      }
    }

    this.submitForm = this.submitForm.bind(this)
    this.handleTextChange = this.handleTextChange.bind(this)
  }

  componentDidMount() {
    this.props.dispatch(fetchSettings())
      .catch(() => {
        Alert.error('Could not retrieve settings.')
      })
  }

  componentWillReceiveProps({ settings }) {
    if (!Object.keys(settings).length) {
      return
    }

    this.setState({
      pristine: true,
      name: {
        value: settings.name,
        valid: true
      },
      upper: {
        value: settings.upper,
        valid: true
      },
      lower: {
        value: settings.lower,
        valid: true
      },
      interval: {
        value: settings.interval,
        valid: true
      }
    })
  }

  submitForm(event) {
    event.preventDefault()

    if (!this.state.name.valid || !this.state.upper.valid || !this.state.lower.valid || !this.state.interval.valid) {
      return
    }

    let fd = new FormData()

    fd.append('name', this.state.name.value)
    fd.append('upper', this.state.upper.value)
    fd.append('lower', this.state.lower.value)
    fd.append('interval', this.state.interval.value)

    this.props.dispatch(submitSettings(fd))
      .then(() => {
        Alert.success('Settings saved successfully.');
      })
      .catch(() => {
        Alert.error('Could not save settings.')
      })
  }

  handleTextChange(key, value, valid) {
    this.setState({
      pristine: false,
      [key]: { value, valid }
    })
  }

  render() {
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
                           className={`text-input full-width spaced ${!this.state.name.valid ? 'is-invalid' : ''}`}
                           placeholder="Give your plant a nice name"
                           value={this.state.name.value}
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
                           className={`text-input full-width spaced ${!this.state.upper.valid ? 'is-invalid' : ''}`}
                           placeholder="Enter the upper limit in percent (default 80)"
                           value={this.state.upper.value}
                           min="0"
                           max="100"
                           onChange={ev => this.handleTextChange('upper', +ev.target.value, +ev.target.value % 1 === 0 && +ev.target.value >= 0 && +ev.target.value <= 100 && +ev.target.value > this.state.lower.value)} />
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
                           className={`text-input full-width spaced ${!this.state.lower.valid ? 'is-invalid' : ''}`}
                           placeholder="Enter the lower limit in percent (default 40)"
                           value={this.state.lower.value}
                           min="0"
                           max="100"
                           onChange={ev => this.handleTextChange('lower', +ev.target.value, +ev.target.value % 1 === 0 && +ev.target.value >= 0 && +ev.target.value <= 100 && +ev.target.value < this.state.upper.value)} />
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
                           className={`text-input full-width spaced ${!this.state.interval.valid ? 'is-invalid' : ''}`}
                           placeholder="Enter measurement interval in minutes (default 60)"
                           value={this.state.interval.value}
                           min="0"
                           onChange={ev => this.handleTextChange('interval', +ev.target.value, +ev.target.value % 1 === 0 && +ev.target.value > 0)} />
                    <p>
                      The measurement interval determines the regularity at which the soil moisture percentage is measured. An interval of 60 minutes or more is recommended to ensure optimal performance. A number greater than 0 is expected.
                    </p>
                  </div>
                </div>
                <input type="submit"
                       data-button="block"
                       disabled={!this.state.name.valid || !this.state.upper.valid || !this.state.lower.valid || !this.state.interval.valid || this.state.pristine || this.props.isSubmitting}
                       value={this.props.isSubmitting ? 'Saving…' : 'Save settings'}
                       onClick={this.submitForm} />
              </form>
            </div>
          </section>
        </div>
        <Alert stack={{limit: 3}} timeout={2000} effect="slide" position="bottom" />
      </main>
    )
  }
}

const mapStateToProps = state => ({
  settings: state.settings.data,
  isFetching: state.settings.isFetching,
  isSubmitting: state.settings.isSubmitting
})

export default connect(mapStateToProps)(Settings)
