/**
 * API middleware to facilitate calling the API and dispatching actions for the
 * request and the successful or erroneous response. The action must contain the
 * actions to be dispatched for each step and API code to be called.
 *
 * @return {Promise} The Promise returned by the API call.
 */
const api = ({ dispatch, getState }) => next => action => {
  const { actions, apiCall, payload = {}, successCallback = () => {}, errorCallback = () => {} } = action

  // Exit early if any of the parameters are invalid.

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

  // Dispatch the request action, followed by a call to the API, which in turn
  // dispatches either a success action or an error action. Errors are re-thrown
  // to allow catching them further down the chain.

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
