/// <reference path="../../typings/tsd.d.ts" />

import * as React from 'react';
import { Link } from 'react-router';
import { connect } from 'react-redux';

import { incrementCounter, decrementCounter, addCounter } from '../actions';
import { CounterList } from './counter_list';
import { browserHistory } from 'react-router';
import * as Cookies from 'cookiejs';

interface IAppState {
  counters: number[];
  token: string;
}

interface IAppProps {
  dispatch?: (func: any) => void;
  counters?: number[];
  token?: string;
}

function select(state: { counters: number[] , token: string }): IAppState {
  return {
    counters: state.counters,
    token: state.token
  };
}


@connect(select)
export class App extends React.Component<IAppProps, {}> {
  public render(): React.ReactElement<{}> {
    const { dispatch, counters , token }: any = this.props;
    return (<div>
        <CounterList counters={counters}
                     increment={(index: number) => dispatch(incrementCounter(index))}
                     decrement={(index: number) => dispatch(decrementCounter(index))}
        />
        <button onClick={() => dispatch(addCounter())}>Add Counter</button>
     </div>
   );
  }
}
