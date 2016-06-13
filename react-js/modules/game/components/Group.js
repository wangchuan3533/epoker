import React from 'react'
const Group = ({x, y, scale, children}) => (
  <g transform={`translate(${x}, ${y}) scale(${scale})`}>
    {children}
  </g>
)

Group.propTypes = {
  x: React.PropTypes.number,
  y: React.PropTypes.number,
  scale: React.PropTypes.number,
  children: React.PropTypes.node,
}

Group.defaultProps = {
  x: 0,
  y: 0,
  scale: 1,
}

export default Group
