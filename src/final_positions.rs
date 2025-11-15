use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use thiserror::Error;

/// Minimum number of teams to calculate the final positions.
pub const MIN_TEAMS: usize = 4;

/// Minimum number of levels of the play-offs tree to calculate the final positions.
pub const MIN_PLAY_OFFS_TREE_LEVELS: usize = 2;

const _: () = {
    assert!(
        MIN_TEAMS >= MIN_PLAY_OFFS_TREE_LEVELS,
        "MIN_TEAMS is less than MIN_PLAY_OFFS_TREE_LEVELS"
    );
};

/// A team.
pub trait Team {
    /// Called at the end of the final position calculation with the team's position.
    fn final_position_callback(&mut self, position: i32);

    /// The final position of the team. It should return [`None`] until [`Team::final_position_callback`] is called,
    /// then the provided position.
    fn final_position(&self) -> Option<i32>;

    /// The general standings position of the team.
    fn general_standings_position(&self) -> i32;
}

/// A duel.
pub trait Duel {
    /// The equal team.
    fn equal(&self) -> Option<FinalPositionTeam>;

    /// The opposite team.
    fn opposite(&self) -> Option<FinalPositionTeam>;

    /// Points made by the equal team.
    fn equal_points(&self) -> Option<i32>;

    /// Points made by the opposite team.
    fn opposite_points(&self) -> Option<i32>;
}

/// A PlayOffs duel.
pub trait PlayOffsDuel: Duel {
    /// Returns whether the duel is a bye.
    fn is_bye(&self) -> bool {
        self.equal().is_none() || self.opposite().is_none()
    }
}

#[derive(Debug)]
/// Final positions calculation builder.
pub struct FinalPositionsBuilder<D: PlayOffsDuel, DUELS: AsMut<[D]>, TFD: Duel, T: Team> {
    teams: Vec<T>,
    play_offs_duels: Vec<DUELS>,
    third_fourth_duel: Option<TFD>,
    _phantom: PhantomData<[D]>,
}

impl<D: PlayOffsDuel, DUELS: AsMut<[D]>, TFD: Duel, T: Team> FinalPositionsBuilder<D, DUELS, TFD, T> {
    /// Creates a new `FinalPositionsBuilder`.
    ///
    /// Returns a tuple containing the play-offs tree (divided in levels) and the 3rd-4th place duel.
    pub fn new<F>(f: F) -> Self
    where
        F: for<'a, 'b> FnOnce(&'a mut DuelsBuilder<'b, T>) -> (Vec<DUELS>, TFD),
    {
        let mut builder = Self {
            teams: Vec::with_capacity(32),
            play_offs_duels: Vec::new(),
            third_fourth_duel: None,
            _phantom: PhantomData,
        };
        let mut final_pos_builder = DuelsBuilder {
            teams: &mut builder.teams,
        };
        let (play_offs_duels, third_fourth_duel) = f(&mut final_pos_builder);
        builder.play_offs_duels = play_offs_duels;
        builder.third_fourth_duel = Some(third_fourth_duel);
        builder
    }

    #[inline]
    /// Evaluates the final positions.
    pub fn evaluate(self) -> Result<(), FinalPositionsError> {
        crate::final_positions::evaluate(self)
    }
}

#[derive(Debug)]
/// Duels builder.
pub struct DuelsBuilder<'builder, T: Team> {
    teams: &'builder mut Vec<T>,
}

impl<T: Team> DuelsBuilder<'_, T> {
    /// Adds a new team to the builder.
    ///
    /// For maximum performance, teams should be added ordered by their general standings positions (1st, 2nd, ...).
    #[inline]
    pub fn add_team(&mut self, team: T) -> FinalPositionTeam {
        self.teams.push(team);
        FinalPositionTeam {
            index: self.teams.len() - 1,
        }
    }
}

#[derive(Error, Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
#[non_exhaustive]
pub enum FinalPositionsError {
    /// Not enough teams to calculate the final positions (see [`MIN_TEAMS`]).
    #[error("not enough teams to calculate the final positions ({0} needed, but {1} were provided)")]
    NotEnoughTeams(usize, usize),
    /// Not enough play-offs tree levels to calculate the final positions (see [`MIN_PLAY_OFFS_TREE_LEVELS`]).
    #[error("not enough play-offs tree levels to calculate the final positions ({0} needed, but {1} were provided)")]
    NotEnoughPlayOffsTreeLevels(usize, usize),
    /// An error occurred while calculating the final positions.
    #[error("an error occurred while calculating final positions: {0}")]
    InternalError(&'static str),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
/// A team inside a group.
pub struct FinalPositionTeam {
    index: usize,
}

pub fn evaluate<D: PlayOffsDuel, DUELS: AsMut<[D]>, TFD: Duel, T: Team>(
    mut builder: FinalPositionsBuilder<D, DUELS, TFD, T>,
) -> Result<(), FinalPositionsError> {
    if builder.teams.len() < MIN_TEAMS {
        return Err(FinalPositionsError::NotEnoughTeams(
            MIN_TEAMS,
            builder.teams.len(),
        ));
    }

    let mut teams = VecHelper(builder.teams);
    let mut play_off_results = builder.play_offs_duels.iter_mut().rev().map(|level| level.as_mut());
    let Some(tfp_duel) = builder.third_fourth_duel else {
        unreachable!()
    };

    // Final and semifinals (we don't actually need semifinals, 3/4th place is enough)
    let (Some([final_duel]), Some(_)) = (play_off_results.next(), play_off_results.next()) else {
        return Err(FinalPositionsError::NotEnoughPlayOffsTreeLevels(
            MIN_PLAY_OFFS_TREE_LEVELS,
            builder.play_offs_duels.len(),
        ));
    };

    if final_duel.equal_points() > final_duel.opposite_points() {
        teams.get_mut(final_duel.equal())?.final_position_callback(1);
        teams.get_mut(final_duel.opposite())?.final_position_callback(2);
    } else {
        teams.get_mut(final_duel.equal())?.final_position_callback(2);
        teams.get_mut(final_duel.opposite())?.final_position_callback(1);
    }

    if tfp_duel.equal_points() > tfp_duel.opposite_points() {
        teams.get_mut(tfp_duel.equal())?.final_position_callback(3);
        teams.get_mut(tfp_duel.opposite())?.final_position_callback(4);
    } else {
        teams.get_mut(tfp_duel.equal())?.final_position_callback(4);
        teams.get_mut(tfp_duel.opposite())?.final_position_callback(3);
    }

    let mut pos = 5;
    for level in play_off_results {
        // One of the two players already has a final position
        #[cfg(debug_assertions)]
        {
            for duel in &*level {
                if duel.is_bye() {
                    continue;
                }

                debug_assert!(
                    teams.get(duel.equal())?.final_position().is_some()
                    || teams.get(duel.opposite())?.final_position().is_some(),
                    "final_position is not already set for a duel"
                );
            }
        }

        level.sort_unstable_by_key(|duel| {
            if duel.is_bye() {
                return None;
            }

            let equal = teams.get(duel.equal()).ok()?;
            let opposite = teams.get(duel.opposite()).ok()?;

            if equal.final_position().is_none() {
                Some(equal.general_standings_position())
            } else {
                Some(opposite.general_standings_position())
            }
        });

        for duel in level {
            if duel.is_bye() {
                continue;
            }

            let equal = teams.get_mut(duel.equal())?;

            if equal.final_position().is_none() {
                equal.final_position_callback(pos);
            } else {
                teams.get_mut(duel.opposite())?.final_position_callback(pos);
            }
            pos += 1;
        }
    }

    // Handle teams excluded from play-offs
    teams.sort_unstable_by_key(|t| t.general_standings_position()); // Should usually already be sorted, so should be fast
    for team in teams.iter_mut() {
        if team.final_position().is_none() {
            team.final_position_callback(pos);
            pos += 1;
        }
    }

    Ok(())
}

const EXPECTED_TEAM: FinalPositionsError = FinalPositionsError::InternalError("Expected team, but found None");
const INVALID_TEAM: FinalPositionsError = FinalPositionsError::InternalError("Could not find team");

#[repr(transparent)]
struct VecHelper<T>(Vec<T>);

impl<T> VecHelper<T> {
    #[inline]
    fn get(&self, index: Option<FinalPositionTeam>) -> Result<&T, FinalPositionsError> {
        self.0.get(index.ok_or(EXPECTED_TEAM)?.index).ok_or(INVALID_TEAM)
    }

    #[inline]
    fn get_mut(&mut self, index: Option<FinalPositionTeam>) -> Result<&mut T, FinalPositionsError> {
        self.0.get_mut(index.ok_or(EXPECTED_TEAM)?.index).ok_or(INVALID_TEAM)
    }
}

impl<T> Deref for VecHelper<T> {
    type Target = Vec<T>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for VecHelper<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// TRAIT DEFAULT IMPLEMENTATIONS

impl<T: Team> Team for &mut T {
    #[inline]
    fn final_position_callback(&mut self, position: i32) {
        (**self).final_position_callback(position);
    }

    #[inline]
    fn final_position(&self) -> Option<i32> {
        (**self).final_position()
    }

    #[inline]
    fn general_standings_position(&self) -> i32 {
        (**self).general_standings_position()
    }
}

impl<D: Duel> Duel for &D {
    #[inline]
    fn equal(&self) -> Option<FinalPositionTeam> {
        (**self).equal()
    }

    #[inline]
    fn opposite(&self) -> Option<FinalPositionTeam> {
        (**self).opposite()
    }

    #[inline]
    fn equal_points(&self) -> Option<i32> {
        (**self).equal_points()
    }

    #[inline]
    fn opposite_points(&self) -> Option<i32> {
        (**self).opposite_points()
    }
}

impl<D: PlayOffsDuel> PlayOffsDuel for &D {
    #[inline]
    fn is_bye(&self) -> bool {
        (**self).is_bye()
    }
}

impl<D: Duel> Duel for &mut D {
    #[inline]
    fn equal(&self) -> Option<FinalPositionTeam> {
        (**self).equal()
    }

    #[inline]
    fn opposite(&self) -> Option<FinalPositionTeam> {
        (**self).opposite()
    }

    #[inline]
    fn equal_points(&self) -> Option<i32> {
        (**self).equal_points()
    }

    #[inline]
    fn opposite_points(&self) -> Option<i32> {
        (**self).opposite_points()
    }
}

impl<D: PlayOffsDuel> PlayOffsDuel for &mut D {
    #[inline]
    fn is_bye(&self) -> bool {
        (**self).is_bye()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fmt::{Display, Formatter};
    use itertools::Itertools;
    use rand::prelude::SliceRandom;
    use rand::thread_rng;

    #[test]
    fn test_final_positions() {
        // Run with --nocapture

        let mut teams = test_teams();
        let builder = make_builder(&mut teams, |teams| {
            let &[a, b, c, d, e, f, ..] = teams else {
                unreachable!();
            };

            (
                vec![
                    vec![
                        ConcreteDuel::new_bye(Some(a), None),
                        ConcreteDuel::new(e, d, 10, 0),
                        ConcreteDuel::new(c, f, 0, 10),
                        ConcreteDuel::new_bye(None, Some(b)),
                    ],
                    vec![
                        ConcreteDuel::new(a, e, 10, 0),
                        ConcreteDuel::new(f, b, 10, 0),
                    ],
                    vec![
                        ConcreteDuel::new(a, f, 10, 0),
                    ]
                ],
                ConcreteDuel::new(e, b, 0, 10)
            )
        });
        builder.evaluate().unwrap();

        teams.sort_by_key(|t| t.final_position);
        for team in teams {
            println!("{}", team);
        }
    }

    #[test]
    fn invalid_duels() {
        assert!(MIN_TEAMS < const { test_teams().len() });

        for i in 0..MIN_PLAY_OFFS_TREE_LEVELS {
            let mut teams = test_teams();
            let builder = make_builder(&mut teams, |_| {
                let levels: Vec<_> = (1..(i + 1)).map(|level_size| {
                    // Generate level
                    (0..level_size).map(|_| ConcreteDuel::new_bye(None, None)).collect()
                }).collect();
                (levels, ConcreteDuel::new_bye(None, None))
            });
            assert_eq!(builder.evaluate(), Err(FinalPositionsError::NotEnoughPlayOffsTreeLevels(MIN_PLAY_OFFS_TREE_LEVELS, i)));
        }
    }

    const fn test_teams() -> [ConcreteTeam; 9] {
       [
            ConcreteTeam::new("A", 1),
            ConcreteTeam::new("B", 2),
            ConcreteTeam::new("C", 3),
            ConcreteTeam::new("D", 4),
            ConcreteTeam::new("E", 5),
            ConcreteTeam::new("F", 6),
            // Teams outside play-offs
            ConcreteTeam::new("X", 7),
            ConcreteTeam::new("Y", 8),
            ConcreteTeam::new("Z", 9),
        ]
    }

    fn make_builder(
        teams: &mut [ConcreteTeam; 9],
        f: impl for<'a> FnOnce(&'a [FinalPositionTeam]) -> (Vec<Vec<ConcreteDuel>>, ConcreteDuel)
    ) -> FinalPositionsBuilder<ConcreteDuel, Vec<ConcreteDuel>, ConcreteDuel, &mut ConcreteTeam> {
        teams.shuffle(&mut thread_rng());
        let builder = FinalPositionsBuilder::new(|builder| {
            let teams: Vec<_> = teams
                .iter_mut()
                .map(|t| (t.group_final_position, builder.add_team(t)))
                // Sort the iterator to make sure we build the intended play-offs tree for the test
                .sorted_unstable_by_key(|t| t.0)
                .map(|t| t.1)
                .collect();
            f(&teams)
        });
        builder
    }

    #[derive(Debug, Eq, PartialEq)]
    struct ConcreteTeam {
        name: &'static str,
        group_final_position: i32,
        final_position: Option<i32>,
    }

    impl ConcreteTeam {
        const fn new(name: &'static str, group_final_position: i32) -> Self {
            ConcreteTeam {
                name,
                group_final_position,
                final_position: None,
            }
        }
    }

    impl Team for ConcreteTeam {
        fn final_position_callback(&mut self, position: i32) {
            self.final_position = Some(position);
        }

        fn final_position(&self) -> Option<i32> {
            self.final_position
        }

        fn general_standings_position(&self) -> i32 {
            self.group_final_position
        }
    }

    impl Display for ConcreteTeam {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "Team {} (final_pos: {:?}, group_final_pos: {})",
                self.name, self.final_position, self.group_final_position,
            )
        }
    }

    #[derive(Debug, Eq, PartialEq)]
    struct ConcreteDuel {
        equal: Option<FinalPositionTeam>,
        opposite: Option<FinalPositionTeam>,
        equal_points: Option<i32>,
        opposite_points: Option<i32>,
    }

    impl ConcreteDuel {
        fn new(
            equal: FinalPositionTeam,
            opposite: FinalPositionTeam,
            equal_points: i32,
            opposite_points: i32
        ) -> Self {
            ConcreteDuel {
                equal: Some(equal),
                opposite: Some(opposite),
                equal_points: Some(equal_points),
                opposite_points: Some(opposite_points),
            }
        }

        fn new_bye(
            equal: Option<FinalPositionTeam>,
            opposite: Option<FinalPositionTeam>,
        ) -> Self {
            assert!(equal.is_none() || opposite.is_none());
            ConcreteDuel {
                equal,
                opposite,
                equal_points: None,
                opposite_points: None,
            }
        }
    }

    impl Duel for ConcreteDuel {
        fn equal(&self) -> Option<FinalPositionTeam> {
            self.equal
        }

        fn opposite(&self) -> Option<FinalPositionTeam> {
            self.opposite
        }

        fn equal_points(&self) -> Option<i32> {
            self.equal_points
        }

        fn opposite_points(&self) -> Option<i32> {
            self.opposite_points
        }
    }

    impl PlayOffsDuel for ConcreteDuel {
    }
}
