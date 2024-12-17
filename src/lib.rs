#![forbid(unsafe_code)]

pub mod traits;
pub mod groups;

/// Minimum number of players in a tournament.
pub const MIN_PLAYER_NUMBER: usize = groups::MIN_PLAYERS_PER_GROUP;
