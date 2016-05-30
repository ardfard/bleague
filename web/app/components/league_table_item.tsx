import * as React from 'react';

export interface IStanding {
  user: string,
  team: string,
  playing: number,
  draw: number,
  lose: number,
  goalFor: number,
  goalAgainst: number,
  pts: number
}

export interface ILeagueTableItemProp {
  standing: IStanding,
  pos: number
}

export class LeagueTableItem extends React.Component<ILeagueTableItemProp, {}> {
  public render() {
    const { user, team, playing, draw, lose, goalFor, goalAgainst, pts } = this.props.standing;
    return (
      <tr>
        <td>{user} ({team})</td>
        <td>{playing}</td>
        <td>{draw}</td>
        <td>{lose}</td>
      </tr>
      );
  }
}
