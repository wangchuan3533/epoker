import React from 'react'
import {RaisedButton} from 'material-ui'
const buttonStyle = {
  margin: 10
}
const Controls = ({actions}) => (
  <div>
    <RaisedButton style={buttonStyle} label="Call/Check" onClick={() => actions.raise(0)}/>
    <RaisedButton style={buttonStyle} label="Raise" onClick={() => actions.raise(1)}/>
    <RaisedButton style={buttonStyle} label="Fold" onClick={() => actions.fold()}/>
  </div>
)

export default Controls
