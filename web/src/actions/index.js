import axios from 'axios'

export const fetchSettings = () => (dispatch) => {
  dispatch({
    type: 'FETCH_SETTINGS_REQUEST'
  })

  return axios.get('http://localhost:5000/settings')
    .then(res => {
      dispatch({
        type: 'FETCH_SETTINGS_SUCCESS',
        data: res.data
      })
    })
    .catch(err => {
      dispatch({
        type: 'FETCH_SETTINGS_ERROR'
      })
    })


}

export const submitSettings = (data) => (dispatch) => {
  dispatch({
    type: 'SUBMIT_SETTINGS_REQUEST'
  })

  return axios.put('http://localhost:5000/settings', data)
    .then(res => {
      dispatch({
        type: 'SUBMIT_SETTINGS_SUCCESS',
        data: res.data
      })
    })
    .catch(err => {
      dispatch({
        type: 'SUBMIT_SETTINGS_ERROR'
      })
    })
}
