import React from 'react'
import Avatar from 'material-ui/lib/avatar'
import CardList from './CardList'
const styles = {
  root: {
    width: 180,
    height: 100,
    borderStyle: 'dotted',
    borderWidth: 1,
    position: 'relative',
    margin: 20,
  },
  container: {
    left: 0,
    width: 80,
    height: 100,
    borderStyle: 'dotted',
    borderWidth: 1,
    position: 'absolute',
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
  cards: {
    right: 0,
    width: 100,
    height: 80,
    position: 'absolute'
  }
}

const Player = ({name, chips}) => {
  return (
    <div style={styles.root}>
      <div style={styles.container}>
        <div style={styles.headBar}>
          {name}
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
      <div style={styles.cards}>
        <CardList cards={[{key: 1, card: 1}, {key: 2, card: 2}]} />
      </div>
    </div>
  )
}

export default Player
