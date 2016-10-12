import axios from 'axios'

export const fetchSettings = () => (dispatch) => {
  dispatch({
    type: 'FETCH_SETTINGS_REQUEST'
  })

  axios.get('/settings')
    .then(res => {
      dispatch({
        type: 'FETCH_SETTINGS_SUCCESS',
        data: res
      })
    })
    .catch(err => {
      console.log(err)
      dispatch({
        type: 'FETCH_SETTINGS_ERROR'
      })
    })
}
