import React from 'react'
import Header from '../components/Header'

const Settings = () => (
  <main className="site">
    <Header />
    <div className="wrap">
      <section className="widget spaced">
        <h2 className="widget-title">Settings</h2>
        <div className="widget-body">
          <form className="settings">
            <div data-grid>
              <div data-col="L1-4">
                <label htmlFor="name" className="spaced">Name</label>
              </div>
              <div data-col="L3-4">
                <input id="name" type="text" className="text-input full-width spaced" placeholder="Give your plant a nice name" />
              </div>
              <hr className="separator" />
              <div data-col="L1-4">
                <label htmlFor="upper" className="spaced">Upper limit</label>
              </div>
              <div data-col="L3-4">
                <input id="upper" type="text" className="text-input full-width spaced" placeholder="Enter the upper limit in percent (default 80)" />
                <p>The upper limit determines at which percentage of soil moisture watering should be disabled. Manual watering automatically stops at this level if it is not manually stopped.</p>
              </div>
              <div data-col="L1-4">
                <label htmlFor="lower" className="spaced">Lower limit</label>
              </div>
              <div data-col="L3-4">
                <input id="lower" type="text" className="text-input full-width spaced" placeholder="Enter the lower limit in percent (default 40)" />
                <p>The lower limit determines over which percentage of soil moisture the plant should always be kept. Automatic watering will always attempt to keep the moisture above this level.</p>
              </div>
              <div data-col="L1-4">
                <label htmlFor="interval" className="spaced">Measurement Interval</label>
              </div>
              <div data-col="L3-4">
                <input id="interval" type="text" className="text-input full-width spaced" placeholder="Enter measurement interval in minutes (default 60)" />
                <p>The measurement interval determines the regularity at which the soil moisture percentage is measured. An interval of 60 minutes or more is recommended to ensure optimal performance.</p>
              </div>
            </div>
            <input type="submit" data-button="block" value="Save settings" />
          </form>
        </div>
      </section>
    </div>
  </main>
)

export default Settings
