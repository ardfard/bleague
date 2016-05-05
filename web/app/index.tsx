/// <reference path="../typings/tsd.d.ts" />

import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Store, createStore } from 'redux';
import { Provider } from 'react-redux';
import { Router, Route, Link, browserHistory, RouterState, RedirectFunction } from 'react-router';

import { App } from './components/app';
import { Login } from './components/login';
import { app } from './reducers';
import * as Cookies from 'cookiejs';

declare const require: (name: String) => any;

interface IHotModule {
  hot?: { accept: (path: string, callback: () => void) => void };
};

declare const module: IHotModule;

function configureStore(): Store {
  const store: Store = createStore(app);

  if (module.hot) {
    module.hot.accept('./reducers', () => {
      const nextRootReducer: any = require('./reducers').app;
      store.replaceReducer(nextRootReducer);
    });
  }
  return store;
}

const store: Store = configureStore();

function requireAuth (nextState : RouterState, replaceState : RedirectFunction) {
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
