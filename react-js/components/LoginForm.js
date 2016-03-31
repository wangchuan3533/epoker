import React from 'react'
import Dialog from 'material-ui/lib/dialog'
import FlatButton from 'material-ui/lib/flat-button'
import TextField from 'material-ui/lib/text-field'

const style = {
  textAlign: 'center',
  width: '30%',
  paddingTop: 100,
}

const LoginForm = ({
  open,
  onLoginClick,
  onRegisterClick
}) => {
  let nameInput, passwordInput
  return (
    <Dialog
      title="Login"
      actions={[
        <FlatButton
          label="Login"
          secondary={true}
          keyboardFocused={true}
          onTouchTap={() => onLoginClick(nameInput.getValue(), passwordInput.getValue())}
        />,
        <FlatButton
          label="Register"
          primary={true}
          onTouchTap={() => onRegisterClick(nameInput.getValue(), passwordInput.getValue())}
        />,
      ]}
      modal={true}
      open={open}
      contentStyle={style}
    >
    <TextField
      hintText="User Name"
      floatingLabelText="User Name"
      ref={(node) => nameInput = node}
    /><br/>
    <TextField
      hintText="Password Field"
      floatingLabelText="Password"
      type="password"
      ref={(node) => passwordInput = node}
    /><br/>
    </Dialog>
  )
}

export default LoginForm
