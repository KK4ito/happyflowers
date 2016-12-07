import React from 'react'
import { connect } from 'react-redux'
import * as actions from '../actions'

/**
 * Class used for handling app-wide connection to the WebSockets server.
 *
 * @extends React.Component
 */
class Connector extends React.Component {

  /**
   * Create a Connector component. Sets initial state.
   *
   * @param {object} props - Standard react props to be passed to the parent
   *                         constructor.
   */
  constructor(props) {
    super(props)

    this.state = {
      socket: null
    }
  }

  /**
   * Lifecycle method that is executed whenever the component is mounted.
   * Connects to the WebSockets server and enhances the socket to react to WS
   * messages.
   */
  componentDidMount() {
    const { dispatch } = this.props

    const socket = new WebSocket(`ws://${window.location.hostname}:9160/`)

    // Connect to the WebSockets server using a somewhat random ID. This ID is
    // sufficiently random for the purposes of this project.

    socket.onopen = () => socket.send(`${+(new Date())}${Math.round(Math.random() * 1000)}`)

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

  /**
   * Lifecycle method that is executed whenever the component is unmounted.
   * Attempts to disconnect the user from the WebSockets server.
   */
  componentWillUnmount() {
    this.state.socket.close()
  }

  /**
   * Renders the component.
   *
   * @return {string} - HTML markup for the component.
   */
  render() {
    return (
      <div>
        {React.cloneElement(this.props.children, { socket: this.state.socket })}
      </div>
    )
  }
}

export default connect()(Connector)
