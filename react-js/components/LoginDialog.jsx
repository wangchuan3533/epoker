import React from 'react';
import Dialog from 'material-ui/lib/dialog';
import FlatButton from 'material-ui/lib/flat-button';
import RaisedButton from 'material-ui/lib/raised-button';
import TextField from 'material-ui/lib/text-field';
import jquery from 'jquery';

const styles = {
  container: {
    textAlign: 'center',
    paddingTop: '200',
  },
  dialog: {
    textAlign: 'center',
    width: '30%',
    paddingTop: 100,
  }
};

export default class LoginDialog extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      open: false,
    };
  }

  handleOpen = () => {
    this.setState({open: true});
  };

  handleClose = () => {
    this.setState({open: false});
  };
  
  handleLogin = () => {
    jquery.ajax({
      type: 'POST',
      url: '/login',
      contentType: 'application/json; charset=utf-8',
      dataType: 'json',
      data: JSON.stringify({
        uid: this.usernameInput.getValue(),
        password: this.passwordInput.getValue(),
      }),
      success: (data) => console.log(data),
      failure: (err) => console.log(err),
    });
  };

  render() {
    const actions = [
      <FlatButton
        label="Login"
        secondary={true}
        keyboardFocused={true}
        onTouchTap={this.handleLogin}
      />,
      <FlatButton
        label="Cancel"
        primary={true}
        onTouchTap={this.handleClose}
      />,
    ];
    return (
      <div style={styles.container}>
        <RaisedButton label="Login" onTouchTap={this.handleOpen} />
        <Dialog
          title="Login"
          actions={actions}
          modal={true}
          open={this.state.open}
          contentStyle={styles.dialog}
        >
        <TextField
          hintText="User Name"
          floatingLabelText="User Name"
          ref={(c) => this.usernameInput = c}
        /><br/>
        <TextField
          hintText="Password Field"
          floatingLabelText="Password"
          type="password"
          ref={(c) => this.passwordInput = c}
        /><br/>
        </Dialog>
      </div>
    );
  }
}
