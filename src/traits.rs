/// A team of a tournament.
pub trait Team {
    /// Returns an estimation of the strength of the team. Should be in the range 0-10.
    fn strength(&self) -> i32;
}
