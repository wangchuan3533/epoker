import React from 'react'
import { connect } from 'react-redux'
import PlayerList from '../components/PlayerList'

const mapStateToProps = (state) => {
  console.log(state.game.players)
  return {
    players: state.game.get('players') || []
  }
}

export default connect(mapStateToProps)(PlayerList)
