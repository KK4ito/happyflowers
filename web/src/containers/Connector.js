import React from 'react'
import { connect } from 'react-redux'
import Alert from 'react-s-alert'
import { List } from 'immutable'
import * as actions from '../actions'
import '../components/Alert.css'

/**
 * Class used for handling app-wide connection to the WebSockets server.
 *
 * @extends React.Component
 */
class Connector extends React.Component {
  constructor(props) {
    super(props)

    this.state = {
      socket: null
    }

    this.displayed = List()
  }

  componentDidMount() {
    const { dispatch } = this.props

    dispatch(actions.fetchHistoryRequest())

    const socket = new WebSocket(`ws://${window.location.hostname}:9160/`)

    // Connect to the WebSockets server using a somewhat random ID. This ID is
    // sufficiently random for the purposes of this project.

    socket.onopen = () => socket.send(`${+(new Date())}${Math.round(Math.random() * 1000)}`)

    // Notify users about missing WS connection.

    socket.onclose = ({ code }) => {
      if (code === 1006) {
        dispatch(actions.addNotification({
          id: 'se',
          text: 'Could not connect to the WebSockets server.',
          type: 'error'
        }))

        dispatch(actions.fetchHistoryError())
      }
    }

    // Handle messages based on their type property.

    socket.onmessage = event => {
      try {
        const msg = JSON.parse(event.data)

        switch (msg.type) {
          case 'measurementReceived':
            dispatch(actions.measurementReceived(msg.payload))
            break
          case 'eventReceived':
            dispatch(actions.eventReceived(msg.payload))
            break
          case 'historyReceived':
            dispatch(actions.fetchHistorySuccess({ res: { data: msg.payload }}))
            break
          case 'settingsChanged':
            dispatch(actions.fetchSettingsSuccess({ res: { data: msg.payload } }))
            break
          case 'busy':
            dispatch(actions.busy(msg.payload))
            break
          default:
            break
        }
      } catch (e) {}
    }

    this.setState({
      socket
    })
  }

  componentWillUnmount() {
    this.state.socket.close()
  }

  componentWillReceiveProps({ notifications }) {
    const newNotifications = notifications.filter(n => !this.displayed.includes(n.get('id')))

    newNotifications.forEach(n => Alert[n.get('type')](n.get('text'), {
      onClose: () => {
        this.props.dispatch(actions.removeNotification(n.get('id')))
        this.displayed = this.displayed.filter(d => d !== n.get('id'))
      }
    }))

    this.displayed = this.displayed.concat(newNotifications.map(n => n.get('id')))
  }

  render() {
    return (
      <div>
        {React.cloneElement(this.props.children, { socket: this.state.socket })}
        <Alert stack={true}
               timeout={2000}
               effect="slide"
               position="bottom" />
      </div>
    )
  }
}

const mapStateToProps = state => ({
  notifications: state.notifications
})

export default connect(mapStateToProps)(Connector)
