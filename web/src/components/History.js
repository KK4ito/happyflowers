import React from 'react'
import Highcharts from 'react-highcharts'
import './History.css'

const chartsOptions = {
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
  series: [{
    animation: false,
    data: [50, 43, 35, 26, 14, 48, 40, 36, 28, 20, 50],
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
  }],
  title: false,
  xAxis: {
    plotLines: [{
      color: 'rgba(0, 0, 0, 0.5)',
      value: 1,
      width: 2,
      zIndex: 2
    }, {
      color: 'rgba(0, 0, 0, 0.5)',
      value: 3,
      width: 2,
      zIndex: 2
    }, {
      color: 'rgba(0, 0, 0, 0.5)',
      value: 5,
      width: 2,
      zIndex: 2
    }, {
      color: 'rgba(0, 0, 255, 0.5)',
      value: 4,
      width: 2,
      zIndex: 2
    }, {
      color: 'rgba(0, 0, 0, 0.5)',
      value: 7,
      width: 2,
      zIndex: 2
    }, {
      color: 'rgba(0, 0, 0, 0.5)',
      value: 9,
      width: 2,
      zIndex: 2
    }]
  },
  yAxis: {
    title: false,
    labels: {
      formatter: function () {
        return this.value + '%'
      }
    }
  }
}

const History = () => (
  <section className="history widget spaced">
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
      <Highcharts config={chartsOptions} />
    </div>
  </section>
)

export default History
