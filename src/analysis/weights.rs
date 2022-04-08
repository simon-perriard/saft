//! Weights representing the resources consumption

use std::cmp::Ordering;
use std::fmt;
use std::ops::*;

#[derive(Debug, Default, Clone, Copy)]
pub struct Weights {
    /// The number of reads
    pub reads: u32,
    /// The number of writes
    pub writes: u32,
    // TODO: add more fields for concrete and symbolic weights
    // TODO: build composite weights
}

impl Weights {
    pub fn new(reads: u32, writes: u32) -> Self {
        Weights { reads, writes }
    }

    pub fn get_total_weight(&self) -> u32 {
        self.reads + self.writes
    }
}

impl fmt::Display for Weights {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(R: {}, W: {})", self.reads, self.writes)
    }
}

impl Add for Weights {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Self {
            reads: self.reads + other.reads,
            writes: self.writes + other.writes,
        }
    }
}

impl AddAssign for Weights {
    fn add_assign(&mut self, other: Self) {
        *self = Self {
            reads: self.reads + other.reads,
            writes: self.writes + other.writes,
        }
    }
}

impl Mul for Weights {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        Self {
            reads: self.reads * other.reads,
            writes: self.writes * other.writes,
        }
    }
}

impl MulAssign for Weights {
    fn mul_assign(&mut self, other: Self) {
        *self = Self {
            reads: self.reads * other.reads,
            writes: self.writes * other.writes,
        }
    }
}

impl PartialEq for Weights {
    fn eq(&self, other: &Self) -> bool {
        self.get_total_weight() == other.get_total_weight()
    }
}

impl Eq for Weights {}

impl Ord for Weights {
    fn cmp(&self, other: &Self) -> Ordering {
        self.get_total_weight().cmp(&other.get_total_weight())
    }
}

impl PartialOrd for Weights {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.get_total_weight()
            .partial_cmp(&other.get_total_weight())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert!(
            Weights::new(1, 2) + Weights::new(3, 4)
                == Weights {
                    reads: 4,
                    writes: 6
                }
        )
    }

    #[test]
    fn test_add_assign() {
        let mut w1 = Weights::new(1, 2);
        let w2 = Weights::new(3, 4);
        w1 += w2;
        assert!(
            w1 == Weights {
                reads: 4,
                writes: 6
            }
        )
    }

    #[test]
    fn test_mul() {
        assert!(
            Weights::new(1, 2) * Weights::new(3, 4)
                == Weights {
                    reads: 3,
                    writes: 8
                }
        )
    }

    #[test]
    fn test_mul_assign() {
        let mut w1 = Weights::new(1, 2);
        let w2 = Weights::new(3, 4);
        w1 *= w2;
        assert!(
            w1 == Weights {
                reads: 3,
                writes: 8
            }
        )
    }

    #[test]
    fn test_max() {
        let w1 = Weights::new(1, 2);
        let w2 = Weights::new(3, 4);

        assert!(Weights::max(w1, w2) == w2)
    }

    #[test]
    fn test_eq() {
        let w1 = Weights::new(1, 2);
        let w2 = Weights::new(2, 1);

        assert!(w1 == w2);
    }

    #[test]
    fn test_ord() {
        let w1 = Weights::new(1, 2);
        let w2 = Weights::new(3, 4);
        assert!(w1 < w2)
    }
}
