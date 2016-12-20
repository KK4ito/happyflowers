import React from 'react'
import IPropTypes from 'react-immutable-proptypes'
import Highcharts from 'react-highcharts'
import { connect } from 'react-redux'
import Widget from '../components/Widget'
import defaultOptions from '../chartOptions'
import { measurementKinds } from '../strings'

// Default settings to use for the charts. See http://www.highcharts.com/docs
// for more information about possible configuration options.

let animating = true

let options = defaultOptions
  .setIn([ 'yAxis', 'min' ], 15)
  .setIn([ 'yAxis', 'max' ], 30)
  .setIn([ 'yAxis', 'labels', 'formatter' ], function () {
    return this.value + ' °C'
  })

/**
 * Functional component representing the temperature history widget, i.e. the
 * widget containing historical data about the temperature levels of the flower.
 *
 * @param {object} props - Standard React props.
 *
 * @return {string} - HTML markup for the component.
 */
const Temperature = ({ measurements, isFetching, socket }) => {

  // Merge the default chart options with options based on the props passed to
  // the component.

  let chartOptions = options

  // History should be animated whenever the data is retrieved for the first
  // time.

  if (!measurements.isEmpty()) {
    animating = false
    chartOptions = chartOptions
      .deleteIn([ 'xAxis', 'min' ])
      .deleteIn([ 'xAxis', 'max' ])
      .set('series', [{
        animation: animating && {
          duration: 1000
        },
        data: measurements.map(m => [ (new Date(m.get('measurementTimestamp'))).getTime(), m.get('measurementValue') ]).toJS(),
        enableMouseTracking: false,
        lineWidth: 1,
        color: '#EBB97D',
        fillColor: {
          linearGradient: { x1: 0, x2: 0, y1: 0, y2: 1 },
          stops: [ [ 0, '#EBB97D' ], [ 1, '#fff' ] ]
        }
      }])
  }

  return (
    <Widget title="Temperature History"
            tooltip="Click and drag the chart to view a section in more detail."
            isLoading={isFetching}>
      <Highcharts config={chartOptions.toJS()} />
    </Widget>
  )
}

Temperature.propTypes = {
  measurements: IPropTypes.list.isRequired,
  isFetching: React.PropTypes.bool
}

const mapStateToProps = state => ({
  measurements: state.history.measurements.filter(m => m.get('measurementKind') === measurementKinds.temperature),
  isFetching: state.history.isFetching
})

export default connect(mapStateToProps)(Temperature)
