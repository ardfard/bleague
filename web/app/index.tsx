
import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Store, createStore, applyMiddleware, compose } from 'redux';
import thunkMiddleware from 'redux-thunk';
import * as createLogger from 'redux-logger';
import { Provider } from 'react-redux';
import { Router, Route, Link, browserHistory } from 'react-router';

import { App, IAppState } from './components/app';
import { Login } from './components/login';
import { app } from './reducers';
import * as storage from './persistence/storage';
import persistenceStore from './persistence/store';

declare const require: (name: String) => any;

interface IHotModule {
  hot?: { accept: (path: string, callback: () => void) => void };
};

declare const module: IHotModule;

const initialState : IAppState = {
  counters: [],
  token: String(storage.get('token'))
}

function configureStore(initialState : IAppState): Store {
  let finalCreateStore = compose(
    applyMiddleware(thunkMiddleware),
    persistenceStore
  )(createStore)

  const store: Store = finalCreateStore(app, initialState);

  if (module.hot) {
    module.hot.accept('./reducers', () => {
      const nextRootReducer: any = require('./reducers').app;
      store.replaceReducer(nextRootReducer);
    });
  }
  return store;
}


const store: Store = configureStore(initialState);

store.subscribe( () =>
  console.log(store.getState())
)

function requireAuth (nextState, replaceState ) {
  const state = store.getState();
  const isLoggedIn = Boolean(state.token);
  if (!isLoggedIn)
    replaceState({
      nextPathname: nextState.location.pathname
    }, '/login');
}

class Main extends React.Component<{}, {}> {
  public render(): React.ReactElement<Provider> {
    return (<Provider store={store}>
      <Router history={browserHistory}>
        <Route path='/' component={App} onEnter={requireAuth} />
        <Route path='/login' component={Login}/>
      </Router>
    </Provider>);
  }
}

ReactDOM.render(<Main />, document.getElementById('app'));
