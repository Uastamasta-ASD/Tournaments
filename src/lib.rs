#![forbid(unsafe_code)]

use serde::{Deserialize, Serialize};

pub mod groups;

/// Minimum number of players in a tournament.
pub const MIN_PLAYER_NUMBER: usize = groups::MIN_PLAYERS_PER_GROUP;

#[derive(Serialize, Deserialize, Default, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[non_exhaustive]
pub struct AdditionalData {
}
