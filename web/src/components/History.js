import React from 'react'
import Highcharts from 'react-highcharts'

const chartsOptions = {
  chart: {
    type: 'areaspline',
  },
  legend: false,
  plotOptions: {
    areaspline: {
      marker: {
        enabled: false
      }
    }
  },
  series: [{
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
          [1, 'rgba(76, 173, 235, 0.05)']
      ]
    }
  }],
  title: false,
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
    <h2 className="widget-title">History</h2>
      <div className="widget-body">
        <Highcharts config={chartsOptions} />
      </div>
  </section>
)

export default History
