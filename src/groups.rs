use crate::{RandGen, Seeder};
use indexmap::IndexMap;
use itertools::Itertools;
use rand::prelude::SliceRandom;
use rand::Rng;
use std::iter::repeat_with;
use std::marker::PhantomData;
use std::mem;
use std::num::NonZero;
use thiserror::Error;

/// Minimum number of teams per group.
pub const MIN_TEAMS_PER_GROUP: usize = 4;

/// A team of a tournament.
pub trait Team {
    /// Returns an estimation of the strength of the team. Should be in the range 0-10.
    fn strength(&self) -> i32;
}

/// Generated groups.
#[derive(Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct Groups<'a, T: Team> {
    /// The generated groups.
    pub groups: Vec<Group<'a, T>>,

    _phantom: PhantomData<()>,
}

impl<T: Team> Clone for Groups<'_, T> {
    #[inline]
    fn clone(&self) -> Self {
        Groups {
            groups: self.groups.clone(),
            _phantom: PhantomData,
        }
    }
}

/// A generated group.
#[derive(Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct Group<'a, T: Team> {
    /// The teams making part of this group.
    pub teams: Vec<&'a T>,

    /// An ordered list of the duels of this group.
    pub duels: Vec<Duel<'a, T>>,

    _phantom: PhantomData<()>,
}

impl<'a, T: Team> Group<'a, T> {
    #[inline]
    fn new() -> Group<'a, T> {
        Group {
            teams: Vec::new(),
            duels: Vec::new(),
            _phantom: PhantomData,
        }
    }
}

impl<T: Team> Clone for Group<'_, T> {
    #[inline]
    fn clone(&self) -> Self {
        Group {
            teams: self.teams.clone(),
            duels: self.duels.clone(),
            _phantom: PhantomData,
        }
    }
}

/// A duel of a generated group.
#[derive(Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct Duel<'a, T: Team> {
    /// Bacchiatore with equal role.
    pub equal: &'a T,
    /// Bacchiatore with opposite role.
    pub opposite: &'a T,

    _phantom: PhantomData<()>,
}

impl<'a, T: Team> Duel<'a, T> {
    #[inline]
    fn new(equal: &'a T, opposite: &'a T) -> Duel<'a, T> {
        Duel {
            equal,
            opposite,
            _phantom: PhantomData,
        }
    }
}

impl<T: Team> Clone for Duel<'_, T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Team> Copy for Duel<'_, T> {}

#[derive(Error, Debug)]
#[non_exhaustive]
pub enum GroupGenError {
    /// Not enough teams for the provided number of groups (see [`MIN_TEAMS_PER_GROUP`]).
    #[error("not enough teams to generate groups ({0} needed, but {1} were provided)")]
    NotEnoughTeams(usize, usize),
    /// An error occurred while generating the groups.
    #[error("an error occurred while generating the groups: {0}")]
    InternalError(&'static str),
}

/// Generates `groups` groups from the provided teams.
pub fn generate_groups<T: Team>(
    teams: &mut [T],
    number_of_groups: NonZero<usize>,
    mut seeder: Seeder,
) -> Result<Groups<T>, GroupGenError> {
    if teams.len() < number_of_groups.get() * MIN_TEAMS_PER_GROUP {
        return Err(GroupGenError::NotEnoughTeams(
            number_of_groups.get() * MIN_TEAMS_PER_GROUP,
            teams.len(),
        ));
    }

    let mut rng: RandGen = seeder.make_rng();

    // Generate groups based on strength
    teams.shuffle(&mut rng);
    teams.sort_by_key(|t| t.strength()); // Keeps the random order (gave by shuffle) if the strength is the same
    let mut groups: Vec<_> = repeat_with(|| Group::new())
        .take(number_of_groups.get())
        .collect();
    let mut iter = groups.iter_mut();
    for team in teams.iter() {
        match iter.next() {
            Some(group) => group.teams.push(team),
            None => {
                iter = groups.iter_mut();
                iter.next().unwrap().teams.push(team); // unwrap() should never panic, since number_of_groups is > 0
            }
        }
    }

    // Add duels to groups
    for Group { teams, duels, .. } in &mut groups {
        teams.shuffle(&mut rng);

        if teams.len() > 4 {
            standard_group_duel_generation(teams, duels)?;
        } else {
            four_teams_group_duel_generation(teams, duels)?;
        }

        // Randomize duel roles
        for duel in duels {
            if rng.gen() {
                mem::swap(&mut duel.equal, &mut duel.opposite);
            }
        }
    }

    Ok(Groups {
        groups,
        _phantom: PhantomData,
    })
}

fn standard_group_duel_generation<'a, 'b: 'a, T: Team>(
    teams: &mut [&'b T],
    duels: &'a mut Vec<Duel<'b, T>>,
) -> Result<(), GroupGenError> {
    // This algorithm makes sure two successive duels don't share any team (to make the players rest between duels)

    debug_assert!(teams.len() > 4);

    // TeamKey -> (TeamKey -> DuelIndex in gen_duels)
    let mut team_duels: IndexMap<_, IndexMap<_, _>> = IndexMap::with_capacity(teams.len());
    let mut gen_duels: Vec<_> = teams
        .iter()
        .copied()
        .tuple_combinations()
        .enumerate()
        .map(|(i, (equal, opposite))| {
            let equal_key = equal as *const _ as usize;
            let opposite_key = opposite as *const _ as usize;
            team_duels
                .entry(equal_key)
                .or_default()
                .insert(opposite_key, i);
            team_duels
                .entry(opposite_key)
                .or_default()
                .insert(equal_key, i);

            Some(Duel::new(equal, opposite))
        })
        .collect();

    // Always choose the duel with the teams that have played fewer duels

    // 0 is never a valid team key (it would have been a NULL ptr before the casts)
    let mut last_duel_team_key = 0;
    let mut last_duel_opponent_key = 0;
    for _ in 0..gen_duels.len() {
        let (&team_key, team_duel_data) = team_duels
            .iter()
            .filter(|(&team_key, _)| {
                team_key != last_duel_team_key && team_key != last_duel_opponent_key
            })
            .filter(|(_, duel_data)| {
                // Ignore teams that only have duels with teams that have just played
                let last_team = duel_data.contains_key(&last_duel_team_key) as usize;
                let last_opponent = duel_data.contains_key(&last_duel_opponent_key) as usize;

                duel_data.len() > last_team + last_opponent
            })
            .max_by_key(|team| team.1.len())
            .ok_or(GroupGenError::InternalError("no duel found"))?;

        let (&opponent_key, &duel_index) = team_duel_data
            .iter()
            .filter(|(&opponent_key, _)| {
                opponent_key != last_duel_team_key && opponent_key != last_duel_opponent_key
            })
            .max_by_key(|(opponent_key, _)| team_duels[*opponent_key].len())
            .ok_or(GroupGenError::InternalError("no duel found for opponent"))?;

        last_duel_team_key = team_key;
        last_duel_opponent_key = opponent_key;
        duels.push(
            gen_duels[duel_index]
                .take()
                .ok_or(GroupGenError::InternalError(
                    "tried to add already inserted duel",
                ))?,
        );

        if let Some(team_duel_data) = team_duels.get_mut(&team_key) {
            team_duel_data.swap_remove(&opponent_key);
            if team_duel_data.is_empty() {
                team_duels.swap_remove(&team_key);
            }
        }
        if let Some(opponent_duel_data) = team_duels.get_mut(&opponent_key) {
            opponent_duel_data.swap_remove(&team_key);
            if opponent_duel_data.is_empty() {
                team_duels.swap_remove(&opponent_key);
            }
        }
    }

    Ok(())
}

fn four_teams_group_duel_generation<'a, 'b: 'a, T: Team>(
    teams: &mut [&'b T],
    duels: &'a mut Vec<Duel<'b, T>>,
) -> Result<(), GroupGenError> {
    // With 4 (or less) teams it's impossible to avoid having two successive
    // duels don't share any team, so using a predetermined order is better

    assert_eq!(teams.len(), 4); // Remove bound checking

    let team1 = teams[0];
    let team2 = teams[1];
    let team3 = teams[2];
    let team4 = teams[3];

    let gen_duels = vec![
        Duel::new(team1, team2),
        Duel::new(team3, team4),
        Duel::new(team1, team3),
        Duel::new(team2, team4),
        Duel::new(team1, team4),
        Duel::new(team2, team3),
    ];

    debug_assert!(duels.is_empty());
    *duels = gen_duels; // duels is empty

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{gen_seed, gen_seeder};
    use std::fmt::{Display, Formatter};
    use std::num::NonZero;

    #[derive(Debug, Clone, Eq, PartialEq)]
    struct ConcreteTeam(&'static str, i32);

    impl Display for ConcreteTeam {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} ({})", self.0, self.1)
        }
    }

    impl Team for ConcreteTeam {
        fn strength(&self) -> i32 {
            self.1
        }
    }

    #[test]
    fn test_groups() {
        // Run with --nocapture

        let mut teams = [
            ConcreteTeam("A", 10),
            ConcreteTeam("B", 10),
            ConcreteTeam("C", 8),
            ConcreteTeam("D", 10),
            ConcreteTeam("E", 10),
            ConcreteTeam("F", 8),
            ConcreteTeam("G", 10),
            ConcreteTeam("H", 8),
            ConcreteTeam("I", 8),
        ];
        let groups = generate_groups(&mut teams, NonZero::new(2).unwrap(), gen_seeder()).unwrap();

        let mut i = 0;
        for group in groups.groups {
            i += 1;
            println!("Group {i}:");
            println!("\tTeams:");
            for team in &group.teams {
                println!("\t\t{}", team);
            }
            println!("\tDuels:");
            for duel in &group.duels {
                println!("\t\t{} -> {}", duel.equal.0, duel.opposite.0);
            }
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
        let teams = [
            ConcreteTeam("A", 10),
            ConcreteTeam("B", 10),
            ConcreteTeam("C", 8),
            ConcreteTeam("D", 10),
            ConcreteTeam("E", 10),
            ConcreteTeam("F", 8),
            ConcreteTeam("G", 10),
            ConcreteTeam("H", 8),
            ConcreteTeam("I", 8),
        ];

        let mut teams_clone = teams.clone();
        let first_groups = generate_groups(
            &mut teams_clone,
            NonZero::new(2).unwrap(),
            Seeder::from(seed),
        )
        .unwrap();

        for _ in 0..50 {
            let mut teams_clone = teams.clone();
            let groups = generate_groups(
                &mut teams_clone,
                NonZero::new(2).unwrap(),
                Seeder::from(seed),
            )
            .unwrap();

            assert_eq!(first_groups, groups);
        }
    }

    #[test]
    fn test_large_groups() {
        for i in MIN_TEAMS_PER_GROUP..=50 {
            let mut teams: Vec<_> = (1..=i).map(|_| ConcreteTeam("team", 5)).collect();
            generate_groups(&mut teams, NonZero::new(1).unwrap(), gen_seeder()).unwrap();
        }
    }
}
