use std::marker::PhantomData;
use std::mem;
use thiserror::Error;

/// Minimum number of teams to generate the play-offs duels.
pub const MIN_TEAMS: usize = 4;

/// A play-off duel.
#[derive(Debug)]
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct PlayOffsDuel<T> {
    /// The equal team. [`None`] if the opposite team automatically passes the turn.
    pub equal: Option<T>,
    /// The opposite team. [`None`] if the equal team automatically passes the turn.
    pub opposite: Option<T>,

    _phantom: PhantomData<()>,
}

impl<T> PlayOffsDuel<T> {
    #[inline]
    fn from(
        mut team1: impl AsMut<Team<T>>,
        mut team2: impl AsMut<Team<T>>,
    ) -> Result<Self, PlayOffsError> {
        use Team::{Empty, Equal, Opposite};

        match (mem::take(team1.as_mut()), mem::take(team2.as_mut())) {
            (Equal(t1), Opposite(t2)) | (Opposite(t2), Equal(t1)) => Ok(PlayOffsDuel {
                equal: Some(t1),
                opposite: Some(t2),
                _phantom: PhantomData,
            }),
            (Equal(t), Empty) | (Empty, Equal(t)) => Ok(PlayOffsDuel {
                equal: Some(t),
                opposite: None,
                _phantom: PhantomData,
            }),
            (Opposite(t), Empty) | (Empty, Opposite(t)) => Ok(PlayOffsDuel {
                equal: None,
                opposite: Some(t),
                _phantom: PhantomData,
            }),
            _ => Err(PlayOffsError::InternalError(
                "invalid teams for duel generation",
            )),
        }
    }
}

impl<T> Default for PlayOffsDuel<T> {
    #[inline(always)]
    fn default() -> Self {
        PlayOffsDuel {
            equal: None,
            opposite: None,
            _phantom: PhantomData,
        }
    }
}

#[derive(Error, Debug)]
#[non_exhaustive]
pub enum PlayOffsError {
    /// Not enough teams to generate the play-offs duels (see [`MIN_TEAMS`]).
    #[error(
        "not enough teams to generate the play-offs duels ({0} needed, but {1} were provided)"
    )]
    NotEnoughTeams(usize, usize),
    /// An error occurred while generating play-offs.
    #[error("an error occurred while generating play-offs: {0}")]
    InternalError(&'static str),
}

/// Generate play-offs from an ordered list of teams.
pub fn generate_play_offs<T>(teams: Vec<T>) -> Result<Vec<PlayOffsDuel<T>>, PlayOffsError> {
    if teams.len() < MIN_TEAMS {
        return Err(PlayOffsError::NotEnoughTeams(MIN_TEAMS, teams.len()));
    }

    let size = teams
        .len()
        .div_ceil(2)
        .checked_next_power_of_two()
        .ok_or(PlayOffsError::InternalError("an overflow occurred"))?;
    let mut play_offs: Vec<PlayOffsDuel<T>> = Vec::with_capacity(size);

    let teams: Vec<_> = teams
        .into_iter()
        .enumerate()
        .map(|(i, team)| Team::from_index(i, team))
        .collect();

    generate_duels(teams, &mut play_offs, size.ilog2())?;

    debug_assert_eq!(play_offs.len(), size);

    Ok(play_offs)
}

fn generate_duels<T>(
    mut teams: Vec<Team<T>>,
    play_offs: &mut Vec<PlayOffsDuel<T>>,
    depth: u32,
) -> Result<(), PlayOffsError> {
    if teams.is_empty() {
        return Err(PlayOffsError::InternalError("no teams provided"));
    }

    if let [team] = &mut *teams {
        // There cannot be "empty" duels, so we must be generating a leaf
        if depth != 0 {
            return Err(PlayOffsError::InternalError("depth is not zero"));
        }

        play_offs.push(PlayOffsDuel::from(team, Team::Empty)?);
    } else if let [team1, team2] = &mut *teams {
        let duel = PlayOffsDuel::from(team1, team2)?;
        if depth == 0 {
            // We are generating a leaf
            play_offs.push(duel);
        } else if depth == 1 {
            // We are in the level before leafs
            play_offs.push(PlayOffsDuel {
                equal: duel.equal,
                opposite: None,
                _phantom: PhantomData,
            });
            play_offs.push(PlayOffsDuel {
                equal: None,
                opposite: duel.opposite,
                _phantom: PhantomData,
            });
        } else {
            // We are in an upper level and would need to generate "empty" duels, which should never happen
            return Err(PlayOffsError::InternalError("depth is greater than one"));
        }
    } else {
        let len = teams.len() / 2;
        let mut teams1 = Vec::with_capacity(len);
        let mut teams2 = Vec::with_capacity(len);

        let mut iter = teams.into_iter();
        let first = iter.next().unwrap(); // We know teams has at least three elements
        let invert = matches!(&first, Team::Opposite(_));
        teams1.push(first);

        for (team, n) in iter.zip((0u8..4u8).cycle()) {
            match n {
                0 | 1 => teams2.push(team),
                2 | 3 => teams1.push(team),
                _ => unreachable!(),
            }
        }

        let next_depth = depth.checked_sub(1).ok_or(PlayOffsError::InternalError(
            "overflow calculating next depth",
        ))?;

        if invert {
            generate_duels(teams2, play_offs, next_depth)?;
            generate_duels(teams1, play_offs, next_depth)?;
        } else {
            generate_duels(teams1, play_offs, next_depth)?;
            generate_duels(teams2, play_offs, next_depth)?;
        }
    }

    Ok(())
}

#[derive(Default)]
enum Team<T> {
    Equal(T),
    Opposite(T),
    #[default]
    Empty,
}

impl<T> Team<T> {
    #[inline]
    fn from_index(index: usize, t: T) -> Self {
        if index % 2 == 0 {
            Self::Equal(t)
        } else {
            Self::Opposite(t)
        }
    }
}

impl<T> AsMut<Team<T>> for Team<T> {
    #[inline(always)]
    fn as_mut(&mut self) -> &mut Team<T> {
        self
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_play_offs() {
        // Run with --nocapture

        let play_offs =
            generate_play_offs(vec!["A", "B", "C", "D", "E", "F", "G", "H", "I", "L", "M"])
                .unwrap();

        let mut i = 1usize;
        for play_off in play_offs {
            print!("{i}\t");
            i += 1;
            if let Some(equal) = play_off.equal {
                println!("{equal}");
            } else {
                println!("-");
            }

            print!("{i}\t");
            i += 1;
            if let Some(opposite) = play_off.opposite {
                println!("{opposite}");
            } else {
                println!("-");
            }
        }
    }

    #[test]
    fn test_reproducibility() {
        let teams: Vec<_> = (0..100).collect();
        let play_offs = generate_play_offs(teams.clone()).unwrap();

        for _ in 0..50 {
            let other_play_offs = generate_play_offs(teams.clone()).unwrap();

            assert_eq!(play_offs, other_play_offs);
        }
    }

    #[test]
    fn test_large_play_offs() {
        for i in MIN_TEAMS..=100 {
            let teams: Vec<_> = (1..=i).map(|_| "team").collect();
            generate_play_offs(teams).unwrap();
        }
    }
}
