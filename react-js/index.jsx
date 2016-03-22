import React from 'react';
import ReactDOM from 'react-dom';
import injectTapEventPlugin from 'react-tap-event-plugin';
injectTapEventPlugin();

import LoginDialog from './components/LoginDialog';

ReactDOM.render(
  <LoginDialog />,
  document.getElementById('app')
);
