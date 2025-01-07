use std::marker::PhantomData;
use std::mem;
use thiserror::Error;

/// Minimum number of teams to generate the play-offs duels.
pub const MIN_TEAMS: usize = 4;

/// A play-off duel.
pub struct PlayOffsDuel<T> {
    /// The equal team. [`None`] if the opposite team automatically passes the turn.
    pub equal: Option<T>,
    /// The opposite team. [`None`] if the equal team automatically passes the turn.
    pub opposite: Option<T>,

    _phantom: PhantomData<()>,
}

impl<T> PlayOffsDuel<T> {
    #[inline]
    fn from(mut team1: impl AsMut<Team<T>>, mut team2: impl AsMut<Team<T>>) -> Self {
        use Team::{Empty, Equal, Opposite};

        match (mem::take(team1.as_mut()), mem::take(team2.as_mut())) {
            (Equal(t1), Opposite(t2)) | (Opposite(t2), Equal(t1)) => PlayOffsDuel {
                equal: Some(t1),
                opposite: Some(t2),
                _phantom: PhantomData,
            },
            (Equal(t), Empty) | (Empty, Equal(t)) => PlayOffsDuel {
                equal: Some(t),
                opposite: None,
                _phantom: PhantomData,
            },
            (Opposite(t), Empty) | (Empty, Opposite(t)) => PlayOffsDuel {
                equal: None,
                opposite: Some(t),
                _phantom: PhantomData,
            },
            _ => unreachable!(), // This method is an impl detail, this should never happen
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
        .checked_next_power_of_two()
        .ok_or(PlayOffsError::InternalError("an overflow occurred"))?;
    let mut play_offs: Vec<PlayOffsDuel<T>> = Vec::with_capacity(size);

    let teams: Vec<_> = teams
        .into_iter()
        .enumerate()
        .map(|(i, team)| Team::from_index(i, team))
        .collect();

    generate_duels(teams, &mut play_offs);

    Ok(play_offs)
}

fn generate_duels<T>(mut teams: Vec<Team<T>>, play_offs: &mut Vec<PlayOffsDuel<T>>) {
    if let [team] = &mut *teams {
        play_offs.push(PlayOffsDuel::from(team, Team::Empty));
    } else if let [team1, team2] = &mut *teams {
        play_offs.push(PlayOffsDuel::from(team1, team2));
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

        if invert {
            generate_duels(teams2, play_offs);
            generate_duels(teams1, play_offs);
        } else {
            generate_duels(teams1, play_offs);
            generate_duels(teams2, play_offs);
        }
    }
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

        let play_offs = generate_play_offs(vec![
            "A", "B", "C", "D", "E", "F", "G", "H", "I", "L", "M", "N", "O",
        ])
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
}
