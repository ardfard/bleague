/// <reference path="../../typings/tsd.d.ts" />

import * as React from 'react';
import { connect } from 'react-redux';

interface ILoginProps {
  tryAuthenticate: () => void;
}

export class Login extends React.Component<ILoginProps, {}> {

  login(e : Event) {
    e.preventDefault();
    this.props.tryAuthenticate()
  }

  public render(): React.ReactElement<{}> {
    return ( <form role="form">
        <div className="form-group">
          <input
           ref="username"
           type="text"
           placeholder="username"/>
          <input
           ref="password"
           type="password"
           placeholder="password"/>
        </div>
        <button type="submit" onClick={this.login.bind(this)}>Login</button>
      </form>
    );
  }
}
