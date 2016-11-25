import React from 'react'
import IPropTypes from 'react-immutable-proptypes'
import Highcharts from 'react-highcharts'
import { connect } from 'react-redux'
import { fromJS } from 'immutable'
import Widget from '../components/Widget'

// Default settings to use for the charts. See http://www.highcharts.com/docs
// for more information about possible configuration options.

const defaultOptions = fromJS({
  chart: {
    type: 'area',
    zoomType: 'x'
  },
  series: [],
  legend: false,
  plotOptions: {
    area: {
      marker: {
        enabled: false
      }
    }
  },
  title: false,
  xAxis: {
    type: 'datetime'
  },
  yAxis: {
    min: 0,
    max: 100,
    title: false,
    labels: {
      formatter: function () {
        return this.value + '%'
      }
    }
  }
})

/**
 * Functional component representing the history widget, i.e. the widget
 * containing historical data about the moisture levels of the flower and
 * relevant events.
 *
 * @param {object} props - Standard React props, destructured to only get the
 *                         events, measurements, settings and isFetching props.
 *
 * @return {string} - HTML markup for the component.
 */
const History = ({ events, measurements, settings, isFetching }) => {

  // Merge the default chart options with options based on the props passed to
  // the component.

  const chartOptions = defaultOptions
    .set('series', [{
      animation: false,
      data: measurements.map(m => [ (new Date(m.get('measurementTimestamp'))).getTime(), m.get('measurementValue') ]).toJS(),
      enableMouseTracking: false,
      lineWidth: 1,
      fillColor: {
        linearGradient: { x1: 0, x2: 0, y1: 0, y2: 1 },
        stops: [ [ 0, '#4cadeb' ], [ 1, '#fff' ] ]
      }
    }])
    .setIn([ 'xAxis', 'minTickInterval' ], 1000 * 60 * settings.get('interval'))
    .setIn([ 'xAxis', 'plotLines' ], events.map(e => ({
      color: e.get('eventType') === 'automatic' ? 'rgba(0, 0, 255, 0.5)' : 'rgba(255, 0, 0, 0.5)',
      value: (new Date(e.get('eventTimestamp'))).getTime(),
      width: 2,
      zIndex: 2
    })))
    .setIn([ 'yAxis', 'plotBands' ], [{
      from: settings.get('lower'),
      to: settings.get('upper'),
      color: 'rgba(129, 235, 76, 0.2)'
    }])

  return (
    <Widget title="History" tooltip="Click and drag the chart to view a section in more detail." isLoading={isFetching}>
        <Highcharts config={chartOptions.toJS()} />
    </Widget>
  )
}

History.propTypes = {
  events: IPropTypes.list.isRequired,
  measurements: IPropTypes.list.isRequired,
  settings: IPropTypes.map.isRequired,
  isFetching: React.PropTypes.bool
}

/**
 * Map Redux state to React props for the Login component.
 *
 * @param {object} state - The Redux state, injected by the <code>connect</code>
 *                         function.
 */
const mapStateToProps = state => ({
  events: state.history.events,
  measurements: state.history.measurements,
  settings: state.settings.data,
  isFetching: state.settings.isFetching || state.settings.isSubmitting
})

export default connect(mapStateToProps)(History)
