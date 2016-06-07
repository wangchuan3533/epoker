import React from 'react'
const Container = ({x, y, children}) => (
  <g transform={`translate(${x}, ${y})`}>
    {children}
  </g>
)

export default Container
