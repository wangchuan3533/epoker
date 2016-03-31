
const login = (state = {authorized: true}, action) => {
  switch (action.type) {
    case 'LOGIN_SUCCESS':
      console.log('login success')
      return {
        ...state,
        authorized: true
      }
    default:
      return state
  }
}

export default login
