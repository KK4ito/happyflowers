import React from 'react'
import IPropTypes from 'react-immutable-proptypes'
import Highcharts from 'react-highcharts'
import { connect } from 'react-redux'
import { Map } from 'immutable'
import Widget from './Widget'

// Default settings to use for the charts. See http://www.highcharts.com/docs
// for more information about possible configuration options.

const defaultOptions = Map({
  chart: {
    type: 'area',
    zoomType: 'x'
  },
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

// Default settings to use for the data series, i.e. the graph marking the
// changing moisture levels.

const defaultSeries = Map({
  animation: false,
  enableMouseTracking: false,
  lineWidth: 1,
  fillColor: {
    linearGradient: {
      x1: 0,
      x2: 0,
      y1: 0,
      y2: 1
    },
    stops: [
      [0, '#4cadeb'],
      [1, '#fff']
    ]
  }
})

// Default settings to use for the vertial lines, i.e. the lines marking the
// manual and automatic watering events.

const defaultLines = Map({
  width: 2,
  zIndex: 2
})

// Default settings to use for the horizontal bands, i.e. the band marking the
// area between lower and upper moisture limits.

const defaultBands = Map({
  color: 'rgba(129, 235, 76, 0.2)'
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
    .set('series', [ defaultSeries
      .set('data', measurements.map(m => [ (new Date(m.get('measurementTimestamp'))).getTime(), m.get('measurementValue') ]).toJS()).toJS()
    ])
    .set('xAxis', Map(defaultOptions.get('xAxis'))
      .set('minTickInterval', 1000 * 60 * settings.get('interval'))
      .set('plotLines', events.map(e => (defaultLines
        .set('color', e.get('eventType') === 'automatic' ? 'rgba(0, 0, 255, 0.5)' : 'rgba(255, 0, 0, 0.5)')
        .set('value', (new Date(e.get('eventTimestamp'))).getTime()).toJS()))
      )
    )
    .set('yAxis', Map(defaultOptions.get('yAxis'))
      .set('plotBands', [ defaultBands
        .set('from', settings.get('lower')).set('to', settings.get('upper')).toJS()
      ])
    )

  return (
    <Widget title="History" tooltip="Click and drag the chart to view a section in more detail." isLoading={isFetching}>
      <div className="widget-body">
        <Highcharts config={chartOptions.toJS()} />
      </div>
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
