
import * as React from 'react';
import { connect } from 'react-redux';
import { login } from '../actions';
import { browserHistory } from 'react-router';

interface ILoginProps {
  tryAuthenticate: (username : string, password : string, redirect?: Function) => void
}

function dispatchToProps( dispatch : Function ) : ILoginProps {
  return {
    tryAuthenticate: (username, pass, redirect?) => dispatch(login(username, pass, redirect))
  }
}

@connect(null, dispatchToProps)
export class Login extends React.Component<ILoginProps, {isLogging: Boolean}> {
  constructor(props) {
    super(props)
    this.state = {
      isLogging: false
    }
  }
  render() {
    let username
    let password
    let isLogging = this.state.isLogging;
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
            this.setState({isLogging: true})
            let nextPath = '/';
            this.props.tryAuthenticate(
              username.value.trim(),
              password.value.trim(),
              () => browserHistory.push(nextPath)
            );
          }}>
          Login
        </button>
        {isLogging ? <div>Logging in</div> : <noscript/>}
     </form>
    );
  }
}
