import { fromJS } from 'immutable'

export default fromJS({
  chart: {
    type: 'area',
    zoomType: 'x'
  },
  series: [[]],
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
    type: 'datetime',
    min: 0,
    max: 0
  },
  yAxis: {
    min: 0,
    max: 100,
    title: false,
    labels: {
      formatter: null
    }
  }
})
