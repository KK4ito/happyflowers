const api = ({ dispatch, getState }) => next => action => {
  const { actions, apiCall, payload = {}, successCallback = () => {}, errorCallback = () => {} } = action

  if (!actions) {
    return next(action)
  }

  if (!Array.isArray(actions) || actions.length !== 3 || !actions.every(a => typeof a === 'function')) {
    throw new Error('Expected an array of three functions.')
  }

  if (typeof apiCall !== 'function') {
    throw new Error('Expected apiCall to be a function.')
  }

  const [ requestAction, successAction, errorAction ] = actions

  dispatch(requestAction(payload))

  return apiCall()
    .then(res => {
      dispatch(successAction({ ...payload, res }))
      successCallback(res)
    })
    .catch(err => {
      dispatch(errorAction({ ...payload, err }))
      errorCallback(err)
      throw err
    })
}

export default api
