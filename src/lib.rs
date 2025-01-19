#![forbid(unsafe_code)]

pub mod groups;
pub mod standings;
pub mod play_offs;

/// Minimum number of teams in a tournament.
pub const MIN_TEAMS_TOURNAMENT: usize = max(groups::MIN_TEAMS_PER_GROUP, play_offs::MIN_TEAMS);

/// Seeder used to seed the RNG. Use the same seed to get reproducible results.
pub type Seeder = rand_seeder::Seeder;

pub(crate) type RandGen = rand_pcg::Pcg64Mcg;

#[cfg(test)]
pub(crate) fn gen_seeder() -> Seeder {
    let seed: [u8; 32] = rand::random();
    Seeder::from(seed)
}

// The stdlib one isn't const
const fn max(n1: usize, n2: usize) -> usize {
    if n1 < n2 {
        n1
    } else {
        n2
    }
}
