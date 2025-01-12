#![forbid(unsafe_code)]

pub mod groups;
pub mod standings;
pub mod play_offs;

/// Minimum number of teams in a tournament.
pub const MIN_TEAMS_TOURNAMENT: usize = groups::MIN_TEAMS_PER_GROUP;
