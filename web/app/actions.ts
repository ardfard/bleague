
export enum ACTION {
    IncrementCounter
  , DecrementCounter
  , AddCounter
  , LoginSuccess }

export interface ICounterAction {
  type: ACTION;
  counterId?: number;
}

export interface ILoginSuccessAction {
  type: ACTION;
  token: string;
}

export function incrementCounter(counterId: number): ICounterAction {
  return { type: ACTION.IncrementCounter, counterId };
}

export function decrementCounter(counterId: number): ICounterAction {
  return { type: ACTION.DecrementCounter, counterId };
}

export function addCounter(): ICounterAction {
  return { type: ACTION.AddCounter };
}


export function login(user: string, password: string, redirect?: Function) : Function {
  return (dispatch) => {
    setTimeout(() => {
      const token = `${user}:${password}`;
      dispatch({
        type: ACTION.LoginSuccess,
        token: token
      });

      if(redirect) redirect();

    }, 300)
  }
}
