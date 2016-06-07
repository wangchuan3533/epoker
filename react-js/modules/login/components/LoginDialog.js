import React from 'react'
import {Dialog, FlatButton} from 'material-ui'
import LoginForm from './LoginForm'
import {getValues} from 'redux-form'

const dialogStyle = {
  textAlign: 'center',
  width: '20%',
  paddingTop: 100,
}

export default ({
  state, actions, form
}) => (
  <Dialog
    title="Login"
    actions={[
      <FlatButton
        label="Login"
        secondary={true}
        keyboardFocused={true}
        onTouchTap={() => actions.login(getValues(form))}
      />,
      <FlatButton
        label="Register"
        primary={true}
        onTouchTap={() => actions.login(getValues(form))}
      />,
    ]}
    modal={true}
    open={state.get('showLoginDialog')}
    contentStyle={dialogStyle}
  >
    <LoginForm />
  </Dialog>
)
