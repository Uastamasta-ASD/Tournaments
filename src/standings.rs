use serde::{Deserialize, Serialize};
use thiserror::Error;

/// A team of a group.
pub trait Team {
    /// Called at the end of the group standings calculation with the team's position and statistics.
    fn group_standings_callback(&mut self, position: i32, statistics: GroupStandingsStatistics);

    /// Called at the end of the general standings calculation with the team's position and statistics.
    fn general_standings_callback(&mut self, position: i32, statistics: GeneralStandingsStatistics);
}

/// A duel of a group.
pub trait Duel {
    /// Points made by the equal team.
    fn equal_points(&self) -> i32;

    /// Points made by the opposite team.
    fn opposite_points(&self) -> i32;
}

#[derive(Serialize, Deserialize, Default, Debug)]
#[non_exhaustive]
/// Statistics of the group standings.
pub struct GroupStandingsStatistics {
    wins: usize,
    losses: usize,
    points_difference: i32,
}

#[derive(Serialize, Deserialize, Default, Debug)]
#[non_exhaustive]
/// Statistics of the general standings.
pub struct GeneralStandingsStatistics {
    wins: usize,
    losses: usize,
    points_difference: i32,
}

#[derive(Debug)]
/// Standings calculation builder.
pub struct StandingsBuilder<D: Duel, T: Team> {
    teams: Vec<(T, TeamData)>,
    groups: Vec<GroupData<D>>,
}

impl<D: Duel, T: Team> StandingsBuilder<D, T> {
    #[inline]
    /// Creates a new `StandingsBuilder`.
    pub fn new() -> StandingsBuilder<D, T> {
        Self {
            teams: Vec::with_capacity(32),
            groups: Vec::with_capacity(4),
        }
    }

    /// Adds a new group to the standings.
    pub fn add_group<F>(&mut self, f: F)
    where
        F: for<'a> FnOnce(&'a mut GroupDuelsBuilder<D, T>),
    {
        let mut group_builder = GroupDuelsBuilder {
            builder: self,
            size: 0,
            duels: Vec::with_capacity(28), // Number of duels for 8 players (32 players / 4 groups = 8 players per group)
        };
        f(&mut group_builder);
        let data = GroupData {
            size: group_builder.size,
            last_position: 0,
            duels: group_builder.duels,
        };
        self.groups.push(data);
    }

    #[inline]
    /// Evaluates the standings.
    pub fn evaluate(self) -> Result<(), StandingsError> {
        crate::standings::evaluate(self)
    }
}

impl<D: Duel, T: Team> Default for StandingsBuilder<D, T> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
/// Group duels builder.
pub struct GroupDuelsBuilder<'builder, D: Duel, T: Team> {
    builder: &'builder mut StandingsBuilder<D, T>,
    size: usize, // Number of teams in this group
    duels: Vec<(D, DuelData)>,
}

impl<D: Duel, T: Team> GroupDuelsBuilder<'_, D, T> {
    /// Adds a new team to the group.
    pub fn add_team(&mut self, team: T) -> StandingsTeam {
        self.builder.teams.push((
            team,
            TeamData {
                group: self.builder.groups.len(),
                wins: 0,
                losses: 0,
                points: 0,
            },
        ));
        self.size += 1;
        StandingsTeam {
            index: self.builder.teams.len() - 1,
        }
    }

    /// Adds a duel to the group.
    pub fn add_duel(&mut self, equal: StandingsTeam, opposite: StandingsTeam, duel: D) {
        self.duels.push((
            duel,
            DuelData {
                equal: equal.index,
                opposite: opposite.index,
            },
        ));
    }
}

#[derive(Error, Debug)]
#[non_exhaustive]
pub enum StandingsError {
    /// An error occurred while calculating standings.
    #[error("an error occurred while calculating standings: {0}")]
    InternalError(&'static str),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
/// A team inside a group.
pub struct StandingsTeam {
    index: usize,
}

#[derive(Debug)]
struct TeamData {
    group: usize, // Index of group
    wins: usize,
    losses: usize,
    points: i32,
}

#[derive(Debug)]
struct GroupData<D: Duel> {
    size: usize,          // Number of teams in this group
    last_position: usize, // Used to calculate a team's position in a group
    duels: Vec<(D, DuelData)>,
}

#[derive(Debug)]
struct DuelData {
    equal: usize,    // Index of equal team
    opposite: usize, // Index of opposite team
}

fn evaluate<D: Duel, T: Team>(mut builder: StandingsBuilder<D, T>) -> Result<(), StandingsError> {
    for group in &mut builder.groups {
        for (duel, duel_data) in &mut group.duels {
            if duel.equal_points() > duel.opposite_points() {
                builder.teams[duel_data.equal].1.wins += 1;
                builder.teams[duel_data.opposite].1.losses += 1;
            } else {
                builder.teams[duel_data.equal].1.losses += 1;
                builder.teams[duel_data.opposite].1.wins += 1;
            }
            builder.teams[duel_data.equal].1.points += duel.equal_points() - duel.opposite_points();
            builder.teams[duel_data.opposite].1.points +=
                duel.opposite_points() - duel.equal_points();
        }
    }

    let mut indexes = (0..builder.teams.len()).collect::<Vec<_>>();
    indexes.sort_by(|&i1, &i2| {
        let (_, team_data_1) = &builder.teams[i1];
        let size_1 = (builder.groups[team_data_1.group].size - 1) as f64;
        let (_, team_data_2) = &builder.teams[i2];
        let size_2 = (builder.groups[team_data_2.group].size - 1) as f64;

        let wins_1 = team_data_1.wins as f64 / size_1;
        let wins_2 = team_data_2.wins as f64 / size_2;
        let losses_1 = team_data_1.losses as f64 / size_1;
        let losses_2 = team_data_2.losses as f64 / size_2;
        let points_1 = team_data_1.points as f64 / size_1;
        let points_2 = team_data_2.points as f64 / size_2;

        wins_1
            .total_cmp(&wins_2)
            .then(losses_1.total_cmp(&losses_2))
            .then(points_1.total_cmp(&points_2))
            .reverse()
    });

    for (i, &mut team_index) in indexes.iter_mut().enumerate() {
        let team = &mut builder.teams[team_index];
        let group = &mut builder.groups[team.1.group];

        team.0.general_standings_callback(
            (i + 1)
                .try_into()
                .map_err(|_| StandingsError::InternalError("couldn't convert index to i32"))?,
            GeneralStandingsStatistics {
                wins: team.1.wins,
                losses: team.1.losses,
                points_difference: team.1.points,
            },
        );

        group.last_position += 1;
        team.0.group_standings_callback(
            group.last_position.try_into().map_err(|_| {
                StandingsError::InternalError("couldn't convert last_position to i32")
            })?,
            GroupStandingsStatistics {
                wins: team.1.wins,
                losses: team.1.losses,
                points_difference: team.1.points,
            },
        );
    }

    Ok(())
}

// TRAIT DEFAULT IMPLEMENTATIONS

impl<T: Team> Team for &mut T {
    #[inline]
    fn group_standings_callback(&mut self, position: i32, duel_data: GroupStandingsStatistics) {
        (**self).group_standings_callback(position, duel_data);
    }

    #[inline]
    fn general_standings_callback(&mut self, position: i32, duel_data: GeneralStandingsStatistics) {
        (**self).general_standings_callback(position, duel_data);
    }
}

impl<D: Duel> Duel for &D {
    #[inline]
    fn equal_points(&self) -> i32 {
        (**self).equal_points()
    }

    #[inline]
    fn opposite_points(&self) -> i32 {
        (**self).opposite_points()
    }
}

impl<D: Duel> Duel for &mut D {
    #[inline]
    fn equal_points(&self) -> i32 {
        (**self).equal_points()
    }

    #[inline]
    fn opposite_points(&self) -> i32 {
        (**self).opposite_points()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fmt::{Display, Formatter};

    #[test]
    fn test_standings() {
        // Run with --nocapture

        let mut team_a = ConcreteTeam::new("A");
        let mut team_b = ConcreteTeam::new("B");
        let mut team_c = ConcreteTeam::new("C");
        let mut team_d = ConcreteTeam::new("D");
        let mut team_e = ConcreteTeam::new("E");
        let mut team_f = ConcreteTeam::new("F");
        let mut team_g = ConcreteTeam::new("G");
        let mut team_h = ConcreteTeam::new("H");

        let mut builder = StandingsBuilder::new();
        builder.add_group(|builder| {
            let team0 = builder.add_team(&mut team_a);
            let team1 = builder.add_team(&mut team_b);
            let team2 = builder.add_team(&mut team_c);
            let team3 = builder.add_team(&mut team_d);
            builder.add_duel(team0, team1, ConcreteDuel::new(5, 1));
            builder.add_duel(team0, team2, ConcreteDuel::new(5, 2));
            builder.add_duel(team0, team3, ConcreteDuel::new(5, 3));
            builder.add_duel(team1, team2, ConcreteDuel::new(5, 2));
            builder.add_duel(team1, team3, ConcreteDuel::new(5, 3));
            builder.add_duel(team2, team3, ConcreteDuel::new(5, 4));
        });
        builder.add_group(|builder| {
            let team0 = builder.add_team(&mut team_e);
            let team1 = builder.add_team(&mut team_f);
            let team2 = builder.add_team(&mut team_g);
            let team3 = builder.add_team(&mut team_h);
            builder.add_duel(team0, team1, ConcreteDuel::new(5, 2));
            builder.add_duel(team0, team2, ConcreteDuel::new(5, 3));
            builder.add_duel(team0, team3, ConcreteDuel::new(5, 4));
            builder.add_duel(team1, team2, ConcreteDuel::new(5, 3));
            builder.add_duel(team1, team3, ConcreteDuel::new(5, 4));
            builder.add_duel(team2, team3, ConcreteDuel::new(5, 4));
        });
        builder.evaluate().unwrap();

        println!("{}", team_a);
        println!("{}", team_b);
        println!("{}", team_c);
        println!("{}", team_d);
        println!("{}", team_e);
        println!("{}", team_f);
        println!("{}", team_g);
        println!("{}", team_h);
    }

    #[derive(Debug)]
    struct ConcreteTeam {
        name: &'static str,
        group_pos_data: Option<(i32, GroupStandingsStatistics)>,
        final_pos_data: Option<(i32, GeneralStandingsStatistics)>,
    }

    impl ConcreteTeam {
        fn new(name: &'static str) -> Self {
            ConcreteTeam {
                name,
                group_pos_data: None,
                final_pos_data: None,
            }
        }
    }

    impl Team for ConcreteTeam {
        fn group_standings_callback(&mut self, position: i32, duel_data: GroupStandingsStatistics) {
            self.group_pos_data = Some((position, duel_data));
        }

        fn general_standings_callback(
            &mut self,
            position: i32,
            duel_data: GeneralStandingsStatistics,
        ) {
            self.final_pos_data = Some((position, duel_data));
        }
    }

    impl Display for ConcreteTeam {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "Team {} (group: {:?}, final: {:?})",
                self.name,
                self.group_pos_data.as_ref().map(|(p, _)| p),
                self.final_pos_data.as_ref().map(|(p, _)| p)
            )
        }
    }

    #[derive(Debug)]
    struct ConcreteDuel {
        equal_points: i32,
        opposite_points: i32,
    }

    impl ConcreteDuel {
        fn new(equal_points: i32, opposite_points: i32) -> Self {
            ConcreteDuel {
                equal_points,
                opposite_points,
            }
        }
    }

    impl Duel for ConcreteDuel {
        fn equal_points(&self) -> i32 {
            self.equal_points
        }

        fn opposite_points(&self) -> i32 {
            self.opposite_points
        }
    }
}
