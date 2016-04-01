import "babel-polyfill"
import React from 'react'
import { render } from 'react-dom'
import { Provider } from 'react-redux'
import thunkMiddleware from 'redux-thunk'
import createLogger from 'redux-logger'
import { createStore, applyMiddleware } from 'redux'
import App from './components/App'
import reducers from './reducers'
import protocols from './protocols'

import injectTapEventPlugin from 'react-tap-event-plugin'
injectTapEventPlugin()


const loggerMiddleware = createLogger()
const store = createStore(
  reducers,
  applyMiddleware(
    thunkMiddleware,
    loggerMiddleware
  )
  
)

render(
  <Provider store={store}>
    <App />
  </Provider>,
  document.getElementById('app')
)
