/// <reference path="../../typings/tsd.d.ts" />

import * as React from 'react';
import { Link } from 'react-router';
import { connect } from 'react-redux';

import { incrementCounter, decrementCounter, addCounter } from '../actions';
import { CounterList } from './counter_list';
import * as Cookies from 'cookiejs';

interface IAppState {
  counters: number[];
}

interface IAppProps {
  dispatch?: (func: any) => void;
  counters?: number[];
}

function select(state: { counters: number[] }): IAppState {
  return {
    counters: state.counters,
  };
}

@connect(select)
export class App extends React.Component<IAppProps, {}> {
  public render(): React.ReactElement<{}> {
    const { dispatch, counters }: any = this.props;

    let auth : string = Cookies.get('_ath');

    return (<div>
        <CounterList counters={counters}
                     increment={(index: number) => dispatch(incrementCounter(index))}
                     decrement={(index: number) => dispatch(decrementCounter(index))}
        />

        <button onClick={() => dispatch(addCounter())}>Add Counter</button>
        <ul>
          <li><Link to="/login">Login</Link></li>
        </ul>
      </div>
    );
  }
}
