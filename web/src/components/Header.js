import React from 'react'
import './Header.css'

const Header = () => (
  <header className="site-header">
    <div data-grid>
      <div data-col="1-2">
        <h1 className="site-title">happy flowers</h1>
      </div>
      <div data-col="1-2" className="text-right">
        <a data-button="secondary" href="">Logout</a>
      </div>
    </div>
  </header>
)

export default Header
