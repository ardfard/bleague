/// <reference path="../../typings/tsd.d.ts" />

import * as React from 'react';
import { connect } from 'react-redux';

export class Login extends React.Component<{}, {}> {

  login(e : Event) {
    e.preventDefault();
    console.log(e);
    // login
  }

  public render(): React.ReactElement<{}> {
    return ( <form role="form">
        <div className="form-group">
          <input type="text" />
          <input type="password" />
        </div>
        <button type="submit" onClick={this.login.bind(this)}>Submit</button>
      </form>
    );
  }
}
