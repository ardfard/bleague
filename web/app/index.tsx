/// <reference path="../typings/tsd.d.ts" />

import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Store, createStore } from 'redux';
import { Provider } from 'react-redux';
import { Router, Route, Link, hashHistory } from 'react-router';

import { App } from './components/app';
import { Login } from './components/login';
import { counterApp } from './reducers';
import * as Cookies from 'cookiejs';

declare const require: (name: String) => any;

interface IHotModule {
  hot?: { accept: (path: string, callback: () => void) => void };
};

declare const module: IHotModule;

function configureStore(): Store {
  const store: Store = createStore(counterApp);

  if (module.hot) {
    module.hot.accept('./reducers', () => {
      const nextRootReducer: any = require('./reducers').counterApp;
      store.replaceReducer(nextRootReducer);
    });
  }
  return store;
}


const store: Store = configureStore();
class Main extends React.Component<{}, {}> {
  public render(): React.ReactElement<Provider> {
    return (<Provider store={store}>
      <Router history={hashHistory}>
        <Route path='/' component={App}/>
        <Route path='/login' component={Login}/>
      </Router>
    </Provider>);
  }
}


ReactDOM.render(<Main />, document.getElementById('app'));
