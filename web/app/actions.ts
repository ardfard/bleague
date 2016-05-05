/// <reference path="../typings/tsd.d.ts" />

export enum ACTION {
    IncrementCounter
  , DecrementCounter
  , AddCounter
  , Login }

export interface ICounterAction {
  type: ACTION;
  counterId?: number;
}

export interface ILoginAction {
  type: ACTION;
  user: string;
  password: string;
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

export function login(user: string, password: string): ILoginAction {
  return { type: ACTION.Login, password: password, user: user };
}
