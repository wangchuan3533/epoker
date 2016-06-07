import "babel-polyfill"
import React from 'react'
import { render } from 'react-dom'
import { Provider } from 'react-redux'
import thunkMiddleware from 'redux-thunk'
import createLogger from 'redux-logger'
import getMuiTheme from 'material-ui/styles/getMuiTheme'
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider'
import { createStore, applyMiddleware, combineReducers } from 'redux'
import { Router, Route, IndexRoute, hashHistory } from 'react-router'
import { syncHistoryWithStore } from 'react-router-redux'
import { routerReducer, routerMiddleware } from 'react-router-redux'
import {reducer as formReducer} from 'redux-form'
import {app, login, game} from './modules'

const reducers = combineReducers({
  app: app.reducer,
  login: login.reducer,
  game: game.reducer,
  routing: routerReducer,
  form: formReducer,
})
const middlewares = applyMiddleware(
  thunkMiddleware,
  createLogger(),
  routerMiddleware(hashHistory)
)

const store = createStore(reducers, middlewares)
const history = syncHistoryWithStore(hashHistory, store)

import injectTapEventPlugin from 'react-tap-event-plugin'
injectTapEventPlugin()
render(
  <Provider store={store}>
    <MuiThemeProvider muiTheme={getMuiTheme()}>
      <Router history={history}>
        <Route path="/" component={app.Main}>
          <IndexRoute component={login.Main} />
          <Route path="/login" component={login.Main}>
            <Route path="/login/game" component={game.Main} />
          </Route>
        </Route>
      </Router>
    </MuiThemeProvider>
  </Provider>,
  document.getElementById('app')
)
