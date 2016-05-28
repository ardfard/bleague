/// <reference path="../../typings/tsd.d.ts" />

import * as React from 'react';
import { connect } from 'react-redux';
import { login } from '../actions';

interface ILoginProps {
  tryAuthenticate: (username : string, password : string) => void
}

function dispatchToProps( dispatch : Function ) : ILoginProps {
  return {
    tryAuthenticate: (username, pass) => dispatch(login(username, pass))
  }
}

@connect(null, dispatchToProps)
export class Login extends React.Component<ILoginProps, {}> {
  render() {
    let username
    let password
    return (
      <form role="form">
        <div className="form-group">
          <input
            ref= {node => {
              username = node
            }}
            type="text"
            placeholder="username"/>
          <input
            ref= {node => {
              password = node
            }}
            type="password"
            placeholder="password"/>
        </div>
        <button
          type="submit"
          onClick={ ( e )=> {
            e.preventDefault();
            this.props.tryAuthenticate(username.value.trim(), password.value.trim());
          }}>
          Login
        </button>
     </form>
    );
  }
}
