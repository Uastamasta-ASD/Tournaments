use crate::{RandGen, Seeder};
use rand::prelude::SliceRandom;
use serde::{Deserialize, Serialize};
use std::fmt::Display;
use std::num::NonZero;
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
#[cfg_attr(test, derive(Eq, PartialEq))]
#[non_exhaustive]
/// Statistics of the group standings.
pub struct GroupStandingsStatistics {
    wins: String,
    points_difference: String,
}

#[derive(Serialize, Deserialize, Default, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
#[non_exhaustive]
/// Statistics of the general standings.
pub struct GeneralStandingsStatistics {
    wins: String,
    points_difference: String,
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
    pub fn evaluate(
        self,
        max_duel_points: NonZero<u16>,
        seeder: Seeder,
    ) -> Result<(), StandingsError> {
        crate::standings::evaluate(self, max_duel_points, seeder)
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

fn evaluate<D: Duel, T: Team>(
    mut builder: StandingsBuilder<D, T>,
    max_duel_points: NonZero<u16>,
    mut seeder: Seeder,
) -> Result<(), StandingsError> {
    for group in &mut builder.groups {
        for (duel, duel_data) in &mut group.duels {
            let equal_adv_points = if duel.equal_points() > duel.opposite_points() {
                builder.teams[duel_data.equal].1.wins += 1;
                1
            } else {
                builder.teams[duel_data.opposite].1.wins += 1;
                -1
            };
            let equal_point_diff = if advantage(duel, max_duel_points) {
                equal_adv_points
            } else {
                duel.equal_points() - duel.opposite_points()
            };
            builder.teams[duel_data.equal].1.points += equal_point_diff;
            builder.teams[duel_data.opposite].1.points += -equal_point_diff;
        }
    }

    let mut rng: RandGen = seeder.make_rng();

    let mut indexes = (0..builder.teams.len()).collect::<Vec<_>>();
    indexes.shuffle(&mut rng);
    indexes.sort_by(|&i1, &i2| {
        let (_, team_data_1) = &builder.teams[i1];
        let size_1 = (builder.groups[team_data_1.group].size - 1) as f64;
        let (_, team_data_2) = &builder.teams[i2];
        let size_2 = (builder.groups[team_data_2.group].size - 1) as f64;

        let wins_1 = team_data_1.wins as f64 / size_1;
        let wins_2 = team_data_2.wins as f64 / size_2;
        let points_1 = team_data_1.points as f64 / size_1;
        let points_2 = team_data_2.points as f64 / size_2;

        wins_1
            .total_cmp(&wins_2)
            .then(points_1.total_cmp(&points_2))
            .reverse()
    });

    for (i, &mut team_index) in indexes.iter_mut().enumerate() {
        let team = &mut builder.teams[team_index];
        let group = &mut builder.groups[team.1.group];

        let duel_number = group.size - 1;
        let wins = team.1.wins as f64 / duel_number as f64;
        let points = team.1.points as f64 / (duel_number * max_duel_points.get() as usize) as f64;

        team.0.general_standings_callback(
            (i + 1)
                .try_into()
                .map_err(|_| StandingsError::InternalError("couldn't convert index to i32"))?,
            GeneralStandingsStatistics {
                wins: format_percentages(wins, false),
                points_difference: format_percentages(points, true),
            },
        );

        group.last_position += 1;
        team.0.group_standings_callback(
            group.last_position.try_into().map_err(|_| {
                StandingsError::InternalError("couldn't convert last_position to i32")
            })?,
            GroupStandingsStatistics {
                wins: format_number(team.1.wins, false),
                points_difference: format_number(team.1.points, true),
            },
        );
    }

    Ok(())
}

#[inline]
fn advantage<D: Duel>(duel: &mut D, max_duel_points: NonZero<u16>) -> bool {
    duel.equal_points() > max_duel_points.get() as i32
        || duel.opposite_points() > max_duel_points.get() as i32
}

#[inline]
fn format_number<N: Display>(number: N, always_show_sign: bool) -> String {
    if always_show_sign {
        format!("{:+}", number)
    } else {
        format!("{}", number)
    }
}

#[inline]
fn format_percentages(percentage: f64, always_show_sign: bool) -> String {
    if always_show_sign {
        format!("{:+.2}%", percentage * 100.0)
    } else {
        format!("{:.2}%", percentage * 100.0)
    }
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
    use crate::{gen_seed, gen_seeder};
    use std::fmt::{Display, Formatter};

    const MAX_POINTS: u16 = 5;

    #[test]
    fn test_standings() {
        // Run with --nocapture

        let mut teams = test_teams();
        let builder = make_builder(&mut teams);
        builder
            .evaluate(NonZero::new(MAX_POINTS).unwrap(), gen_seeder())
            .unwrap();

        for team in teams {
            println!("{}", team);
        }
    }

    fn test_teams() -> [ConcreteTeam; 8] {
        [
            ConcreteTeam::new("A"),
            ConcreteTeam::new("B"),
            ConcreteTeam::new("C"),
            ConcreteTeam::new("D"),
            ConcreteTeam::new("E"),
            ConcreteTeam::new("F"),
            ConcreteTeam::new("G"),
            ConcreteTeam::new("H"),
        ]
    }

    fn make_builder(
        teams: &mut [ConcreteTeam; 8],
    ) -> StandingsBuilder<ConcreteDuel, &mut ConcreteTeam> {
        let mut teams = teams.iter_mut();
        let mut builder = StandingsBuilder::new();
        builder.add_group(|builder| {
            let team0 = builder.add_team(teams.next().unwrap());
            let team1 = builder.add_team(teams.next().unwrap());
            let team2 = builder.add_team(teams.next().unwrap());
            let team3 = builder.add_team(teams.next().unwrap());
            builder.add_duel(team0, team1, ConcreteDuel::new(5, 1));
            builder.add_duel(team0, team2, ConcreteDuel::new(5, 2));
            builder.add_duel(team0, team3, ConcreteDuel::new(5, 3));
            builder.add_duel(team1, team2, ConcreteDuel::new(5, 2));
            builder.add_duel(team1, team3, ConcreteDuel::new(5, 3));
            builder.add_duel(team2, team3, ConcreteDuel::new(6, 4));
        });
        builder.add_group(|builder| {
            let team0 = builder.add_team(teams.next().unwrap());
            let team1 = builder.add_team(teams.next().unwrap());
            let team2 = builder.add_team(teams.next().unwrap());
            let team3 = builder.add_team(teams.next().unwrap());
            builder.add_duel(team0, team1, ConcreteDuel::new(5, 2));
            builder.add_duel(team0, team2, ConcreteDuel::new(5, 3));
            builder.add_duel(team0, team3, ConcreteDuel::new(6, 4));
            builder.add_duel(team1, team2, ConcreteDuel::new(5, 3));
            builder.add_duel(team1, team3, ConcreteDuel::new(6, 4));
            builder.add_duel(team2, team3, ConcreteDuel::new(7, 5));
        });
        builder
    }

    #[derive(Debug, Eq, PartialEq)]
    struct ConcreteTeam {
        name: &'static str,
        group_pos_data: Option<(i32, GroupStandingsStatistics)>,
        general_pos_data: Option<(i32, GeneralStandingsStatistics)>,
    }

    impl ConcreteTeam {
        fn new(name: &'static str) -> Self {
            ConcreteTeam {
                name,
                group_pos_data: None,
                general_pos_data: None,
            }
        }
    }

    impl Team for ConcreteTeam {
        fn group_standings_callback(
            &mut self,
            position: i32,
            statistics: GroupStandingsStatistics,
        ) {
            self.group_pos_data = Some((position, statistics));
        }

        fn general_standings_callback(
            &mut self,
            position: i32,
            statistics: GeneralStandingsStatistics,
        ) {
            self.general_pos_data = Some((position, statistics));
        }
    }

    impl Display for ConcreteTeam {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            let group_data = self
                .group_pos_data
                .as_ref()
                .map(|(p, s)| (p.to_string(), serde_json::to_string(s).unwrap()))
                .unwrap_or(("None".to_owned(), "None".to_owned()));
            let general_data = self
                .general_pos_data
                .as_ref()
                .map(|(p, s)| (p.to_string(), serde_json::to_string(s).unwrap()))
                .unwrap_or(("None".to_owned(), "None".to_owned()));

            write!(
                f,
                "Team {} (group pos: {}, general pos: {}, group stats: {}, general stats: {})",
                self.name, group_data.0, general_data.0, group_data.1, general_data.1,
            )
        }
    }

    #[derive(Debug, Eq, PartialEq)]
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

    #[test]
    fn test_reproducibility() {
        // Execute a bunch of times to test against different seeds
        for _ in 0..50 {
            reproducibility_test_case(gen_seed());
        }
    }

    fn reproducibility_test_case(seed: [u8; 32]) {
        let mut teams = test_teams();
        make_builder(&mut teams)
            .evaluate(NonZero::new(MAX_POINTS).unwrap(), Seeder::from(seed))
            .unwrap();

        for _ in 0..50 {
            let mut teams_clone = test_teams();
            make_builder(&mut teams_clone)
                .evaluate(NonZero::new(MAX_POINTS).unwrap(), Seeder::from(seed))
                .unwrap();

            assert_eq!(teams, teams_clone);
        }
    }
}
