import React from 'react'
import './Login.css'

const Login = () => (
  <form className="login">
    <h1 className="form-title">
      happy flowers
    </h1>
    <input className="text-input full-width spaced"
           type="password"
           placeholder="Enter the password" />
    <input data-button="block"
           type="submit"
           value="Sign in" />
  </form>
)

export default Login
