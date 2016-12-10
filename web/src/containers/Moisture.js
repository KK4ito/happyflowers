import React from 'react'
import IPropTypes from 'react-immutable-proptypes'
import Highcharts from 'react-highcharts'
import { connect } from 'react-redux'
import Widget from '../components/Widget'
import defaultOptions from '../chartOptions'

// Default settings to use for the charts. See http://www.highcharts.com/docs
// for more information about possible configuration options.

let animating = true

let options = defaultOptions
  .setIn([ 'yAxis', 'min' ], 0)
  .setIn([ 'yAxis', 'max' ], 100)
  .setIn([ 'yAxis', 'labels', 'formatter' ], function () {
    return this.value + '%'
  })

/**
 * Functional component representing the history widget, i.e. the widget
 * containing historical data about the moisture levels of the flower and
 * relevant events.
 *
 * @param {object} props - Standard React props.
 *
 * @return {string} - HTML markup for the component.
 */
const Moisture = ({ events, measurements, settings, isFetching, socket }) => {

  // Merge the default chart options with options based on the props passed to
  // the component.

  let chartOptions = options
    .set('series', [{
      animation: animating && {
        duration: 1000
      },
      data: measurements.map(m => [ (new Date(m.get('measurementTimestamp'))).getTime(), m.get('measurementValue') ]).toJS(),
      enableMouseTracking: false,
      lineWidth: 1,
      color: '#4cadeb',
      fillColor: {
        linearGradient: { x1: 0, x2: 0, y1: 0, y2: 1 },
        stops: [ [ 0, '#4cadeb' ], [ 1, '#fff' ] ]
      }
    }])
    .setIn([ 'xAxis', 'plotLines' ], events.map(e => ({
      color: e.get('eventKind') === 'automatic' ? 'rgba(0, 0, 255, 0.5)' : 'rgba(255, 0, 0, 0.5)',
      value: (new Date(e.get('eventTimestamp'))).getTime(),
      width: 2,
      zIndex: 2
    })))
    .setIn([ 'yAxis', 'plotBands' ], [{
      from: settings.get('lower'),
      to: settings.get('upper'),
      color: 'rgba(129, 235, 76, 0.2)'
    }])

  // History should be animated whenever the data is retrieved for the first
  // time.

  if (!measurements.isEmpty()) {
    animating = false
    chartOptions = chartOptions
      .deleteIn([ 'xAxis', 'min' ])
      .deleteIn([ 'xAxis', 'max' ])
  }

  return (
    <Widget title="Moisture History"
            tooltip="Click and drag the chart to view a section in more detail."
            isLoading={isFetching}>
      <Highcharts config={chartOptions.toJS()} />
    </Widget>
  )
}

Moisture.propTypes = {
  events: IPropTypes.list.isRequired,
  measurements: IPropTypes.list.isRequired,
  settings: IPropTypes.map.isRequired,
  isFetching: React.PropTypes.bool
}

const mapStateToProps = state => ({
  events: state.history.events,
  measurements: state.history.measurements.filter(m => m.get('measurementKind') === 'moisture'),
  settings: state.settings.data,
  isFetching: state.settings.isFetching || state.history.isFetching
})

export default connect(mapStateToProps)(Moisture)
