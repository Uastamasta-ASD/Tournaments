use crate::traits::Team;
use itertools::Itertools;
use rand::prelude::SliceRandom;
use rand::{thread_rng, Rng};
use std::array::{from_fn, IntoIter};
use std::mem;
use std::num::NonZero;
use std::slice::{Iter, IterMut};
use thiserror::Error;

/// Minimum number of players per group.
pub const MIN_PLAYERS_PER_GROUP: usize = 4;

/// Generated groups.
pub struct Groups<'a, T: Team, const N: usize> {
    groups: [Group<'a, T>; N],
}

impl<'a, T: Team, const N: usize> Groups<'a, T, N> {
    /// Returns an iterator over the generated groups.
    #[inline]
    pub fn iter<'b>(&'b self) -> Iter<'b, Group<'a, T>> {
        IntoIterator::into_iter(self)
    }

    /// Returns an iterator over the generated groups.
    #[inline]
    pub fn iter_mut<'b>(&'b mut self) -> IterMut<'b, Group<'a, T>> {
        IntoIterator::into_iter(self)
    }
}

impl<'a, 'b, T: Team, const N: usize> IntoIterator for &'b Groups<'a, T, N> {
    type Item = &'b Group<'a, T>;
    type IntoIter = Iter<'b, Group<'a, T>>;

    /// Returns an iterator over the generated groups.
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.groups.iter()
    }
}

impl<'a, 'b, T: Team, const N: usize> IntoIterator for &'b mut Groups<'a, T, N> {
    type Item = &'b mut Group<'a, T>;
    type IntoIter = IterMut<'b, Group<'a, T>>;

    /// Returns an iterator over the generated groups.
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.groups.iter_mut()
    }
}

impl<'a, T: Team, const N: usize> IntoIterator for Groups<'a, T, N> {
    type Item = Group<'a, T>;
    type IntoIter = IntoIter<Group<'a, T>, N>;

    /// Returns an iterator over the generated groups.
    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.groups.into_iter()
    }
}

/// A generated group.
#[derive(Debug)]
pub struct Group<'a, T: Team> {
    /// The teams making part of this group.
    pub teams: Vec<&'a T>,

    /// An ordered list of the duels of this group.
    pub duels: Vec<Duel<'a, T>>,
}

/// A duel of a generated group.
#[derive(Debug)]
pub struct Duel<'a, T: Team> {
    /// Bacchiatore with equal role.
    pub equal: &'a T,
    /// Bacchiatore with opposite role.
    pub opposite: &'a T,
}

impl<'a, T: Team> Group<'a, T> {
    #[inline]
    fn new() -> Group<'a, T> {
        Group {
            teams: Vec::new(),
            duels: Vec::new(),
        }
    }
}

#[derive(Error, Debug)]
#[non_exhaustive]
pub enum GroupGenError {
    /// Not enough teams for the provided number of groups (see [`MIN_PLAYERS_PER_GROUP`]).
    #[error("not enough teams to generate groups ({0} needed, but {1} were provided)")]
    NotEnoughTeams(usize, usize),
}

/// Generates `N` groups from the provided teams.
pub fn generate_groups<T: Team, const N: usize>(
    teams: &mut [T],
) -> Result<Groups<T, N>, GroupGenError> {
    const {
        NonZero::new(N).expect("Number of groups must be greater than zero");
    }

    if teams.len() < N * MIN_PLAYERS_PER_GROUP {
        return Err(GroupGenError::NotEnoughTeams(
            N * MIN_PLAYERS_PER_GROUP,
            teams.len(),
        ));
    }

    let mut rng = thread_rng();

    // Generate groups based on strength
    teams.shuffle(&mut rng);
    teams.sort_by_key(|t| t.strength()); // Keeps the random order (gave by shuffle) if the strength is the same
    let mut groups: [_; N] = from_fn(|_| Group::new());
    let mut iter = groups.iter_mut();
    for team in teams.iter() {
        match iter.next() {
            Some(group) => group.teams.push(team),
            None => {
                iter = groups.iter_mut();
                iter.next().unwrap().teams.push(team); // unwrap() should never panic, since we checked that N > 0
            }
        }
    }

    // Add duels to groups
    for Group { teams, duels } in &mut groups {
        *duels = teams
            .iter()
            .copied()
            .tuple_combinations()
            .map(|(equal, opposite)| Duel { equal, opposite })
            .collect();

        for Duel { equal, opposite } in duels.iter_mut() {
            if rng.gen() {
                mem::swap(equal, opposite);
            }
        }
        // TODO Make sure two successive duels don't share any player (to make them rest between duels)
    }

    Ok(Groups { groups })
}

#[cfg(test)]
mod test {
    use crate::groups::generate_groups;
    use crate::traits::Team;
    use std::fmt::{Display, Formatter};

    #[test]
    fn test_groups() {
        // Run with --nocapture
        #[derive(Debug)]
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

        let mut teams = [
            ConcreteTeam("A", 10),
            ConcreteTeam("B", 10),
            ConcreteTeam("C", 8),
            ConcreteTeam("D", 10),
            ConcreteTeam("E", 10),
            ConcreteTeam("F", 8),
            ConcreteTeam("E", 10),
            ConcreteTeam("F", 8),
        ];
        let groups = generate_groups::<_, 2>(&mut teams).unwrap();

        let mut i = 0;
        for group in groups {
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
}