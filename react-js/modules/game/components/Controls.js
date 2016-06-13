import React from 'react'
import {RaisedButton} from 'material-ui'
const buttonStyle = {
  margin: 10
}
const Controls = () => (
  <div>
    <RaisedButton style={buttonStyle} label="Call/Check" />
    <RaisedButton style={buttonStyle} label="Raise" />
    <RaisedButton style={buttonStyle} label="Fold" />
  </div>
)

export default Controls
