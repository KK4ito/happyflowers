import React from 'react'
import Highcharts from 'react-highcharts'

const chartsOptions = {
  chart: {
    type: 'area'
  },
  legend: false,
  title: false,
  series: [{
    animation: false,
    data: [50, 43, 35, 26, 14, 48, 40, 36, 28, 20, 50],
    enableMouseTracking: false
  }],
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
