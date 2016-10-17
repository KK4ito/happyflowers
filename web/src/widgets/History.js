import React from 'react'
import Highcharts from 'react-highcharts'
import { connect } from 'react-redux'
import { Map } from 'immutable'
import Widget from './Widget'

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

const defaultLines = Map({
  width: 2,
  zIndex: 2
})

const defaultBands = Map({
  color: 'rgba(129, 235, 76, 0.2)'
})

const History = ({ events, measurements, settings, isFetching }) => {
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

const mapStateToProps = state => ({
  events: state.history.events,
  measurements: state.history.measurements,
  settings: state.settings.data,
  isFetching: state.settings.isFetching || state.settings.isSubmitting
})

export default connect(mapStateToProps)(History)
