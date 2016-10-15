import React from 'react'
import './Header.css'

const Header = () => (
  <header className="site-header">
    <div data-grid>
      <div data-col="L3-4">
        <h1 className="site-title">
          <a href="/">
            happy flowers
          </a>
        </h1>
      </div>
      <div data-col="L1-4">
        <div data-grid>
          <div data-col="1-2">
            <a data-button="block secondary"
               href="settings">
              Settings
            </a>
          </div>
          <div data-col="1-2">
            <a data-button="block secondary"
               href="">
              Logout
            </a>
          </div>
        </div>
      </div>
    </div>
  </header>
)

export default Header
