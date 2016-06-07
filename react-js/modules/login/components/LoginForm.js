import React from 'react'
import {reduxForm} from 'redux-form'
import {Divider, TextField} from 'material-ui'

const style = {
  textAlign: 'center',
  width: '30%',
  paddingTop: 100,
}

const fields = [
  'name',
  'password',
]

const Form = ({
  fields
}) => (
  <form>
    <TextField
      {...fields.name}
      hintText="User Name"
      floatingLabelText="User Name"
      fullWidth={true}
      underlineShow={false}
    />
    <Divider />
    <TextField
      {...fields.password}
      hintText="Password"
      floatingLabelText="Password"
      type="password"
      fullWidth={true}
      underlineShow={false}
    />
  </form>
)

Form.propTypes = {
  fields: React.PropTypes.object.isRequired,
}

export default reduxForm({
  form: 'loginForm',
  fields: fields
})(Form)
