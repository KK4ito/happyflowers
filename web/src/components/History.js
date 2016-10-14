import React from 'react'
import Highcharts from 'react-highcharts'
import { connect } from 'react-redux'
import Loader from './Loader'
import './History.css'

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
  color: 'rgba(0, 0, 255, 0.5)',
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
      data: measurements.map(m => m.measurementValue)
    }],
    xAxis: {
      plotLines: events.map(e => ({
        ...defaultLines,
        value: e.eventTimestamp
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
    <section className="history widget spaced">
      <Loader loading={isFetching} />
      <h2 className="widget-title">
        History
        <div className="tooltip">
        <span data-icon="help">
          <span className="tooltip-body">
            Click and drag the chart to view a section in more detail.
          </span>
        </span>
        </div>
      </h2>
      <div className="widget-body">
        <Highcharts config={chartOptions} />
      </div>
    </section>
  )
}

const mapStateToProps = (state) => ({
  events: state.history.events,
  measurements: state.history.measurements,
  settings: state.settings.data,
  isFetching: state.settings.isFetching || state.settings.isSubmitting
})

export default connect(
  mapStateToProps
)(History)
