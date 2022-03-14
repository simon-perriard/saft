use std::fmt;
use std::ops::*;

#[derive(Debug, PartialEq, PartialOrd, Default, Clone, Copy)]
pub struct Weights {
    pub reads: u32,
    pub writes: u32,
}

impl Weights {
    pub fn new(reads: u32, writes: u32) -> Self {
        Weights { reads, writes }
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

pub trait Max {
    fn max(w1: Self, w2: Self) -> Self;
}

impl Max for Weights {
    fn max(w1: Self, w2: Self) -> Self {
        if w1 > w2 {
            Weights { ..w1 }
        } else {
            Weights { ..w2 }
        }
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
}
