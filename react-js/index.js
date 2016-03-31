import "babel-polyfill"
import React from 'react'
import { render } from 'react-dom'
import App from './components/App'
import { Provider } from 'react-redux'
import { createStore } from 'redux'
import reducers from './reducers'

import injectTapEventPlugin from 'react-tap-event-plugin'
injectTapEventPlugin()

let store = createStore(reducers)
render(
  <Provider store={store}>
    <App />
  </Provider>,
  document.getElementById('app')
)
