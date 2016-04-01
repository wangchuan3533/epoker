import React from 'react'
import Avatar from 'material-ui/lib/avatar'
const styles = {
  container: {
    width: 80,
    height: 100,
    borderStyle: 'dotted',
    borderWidth: 1,
    position: 'relative',
  },
  headBar: {
    width: '100%',
    position: 'absolute',
    top: 0,
    margin: 'auto',
    textAlign: 'center'
  },
  avatar: {
    width: '100%',
    position: 'relative',
    margin: 'auto',
    paddingTop: 30,
    textAlign: 'center'
  },
  bottomBar: {
    width: '100%',
    position: 'absolute',
    bottom: 0,
    margin: 'auto',
    textAlign: 'center'
  },
}

const Player = ({status, chips}) => {
  return (
    <div style={styles.container}>
      
      <div style={styles.headBar}>
        {status}
      </div>
      
      <div style={styles.avatar}>
        <Avatar>
          P
        </Avatar>
      </div>
      
      <div style={styles.bottomBar}>
        ${chips}
      </div>
    </div>
  )
}

export default Player
