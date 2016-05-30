import { ACTION } from '../actions';
import * as storage from './storage';

export default function persistenceHandler(next) {
  return (reducer, initialState) => {
    const store = next(reducer, initialState);
    return Object.assign({}, store, {
      dispatch (action) {
        store.dispatch(action)
        if (action.type == ACTION.LoginSuccess)
          storage.put('token', action.token)
      }
    })
  }
}
