#![forbid(unsafe_code)]

pub mod groups;
pub mod standings;
pub mod play_offs;

/// Minimum number of players in a tournament.
pub const MIN_PLAYER_NUMBER: usize = groups::MIN_PLAYERS_PER_GROUP;
