import * as React from 'react';
import { IStanding, LeagueTableItem } from './league_table_item';
import * as R from 'ramda';


export interface ILeagueTableProps {
  standings: IStanding[]
}

export class LeagueTable extends React.Component<ILeagueTableProps, {}> {
  public render() {
    let standings = this.props.standings;
    let items = R.zip(R.range(1,standings.length), standings).map(
      ([pos, standing]) => <LeagueTableItem standing={standing} pos= {pos}/> );

    return (
      <div>
        <table >
          <thead>
            <tr>
              <th>Pos</th>
              <th>Player/Team</th>
              <th>P</th>
              <th>D</th>
              <th>L</th>
              <th>GF</th>
              <th>GA</th>
              <th>GD</th>
              <th>Pts</th>
            </tr>
          </thead>
          <tbody>

          </tbody>
        </table>
      </div>);
  }
}
