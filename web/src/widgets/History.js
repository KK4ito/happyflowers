import React from 'react'
import Highcharts from 'react-highcharts'
import { connect } from 'react-redux'
import Widget from './Widget'
import Loader from '../components/Loader'

const defaultOptions = {
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
}

const defaultSeries = {
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
}

const defaultLines = {
  width: 2,
  zIndex: 2
}

const defaultBands = {
  color: 'rgba(129, 235, 76, 0.2)'
}

const History = ({ events, measurements, settings, isFetching }) => {
  const chartOptions = {
    ...defaultOptions,
    series: [{
      ...defaultSeries,
      data: measurements.map(m => [ (new Date(m.measurementTimestamp)).getTime(), m.measurementValue ])
    }],
    xAxis: {
      ...defaultOptions.xAxis,
      minTickInterval: 1000 * 60 * settings.interval,
      plotLines: events.map(e => ({
        ...defaultLines,
        color: e.eventType === 'automatic' ? 'rgba(0, 0, 255, 0.5)' : 'rgba(255, 0, 0, 0.5)',
        value: (new Date(e.eventTimestamp)).getTime()
      }))
    },
    yAxis: {
      ...defaultOptions.yAxis,
      plotBands: [{
        ...defaultBands,
        from: settings.lower,
        to: settings.upper
      }]
    }
  }

  return (
    <Widget title="History" tooltip={"Click and drag the chart to view a section in more detail."} isLoading={isFetching}>
      <div className="widget-body">
        <Highcharts config={chartOptions} />
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
