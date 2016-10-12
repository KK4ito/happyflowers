import axios from 'axios'

export const fetchSettings = () => (dispatch) => {
  dispatch({
    type: 'FETCH_SETTINGS_REQUEST'
  })

  axios.get('http://localhost:5000/settings')
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
