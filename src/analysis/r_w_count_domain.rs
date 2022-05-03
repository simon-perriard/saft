use crate::size_language::Size;
use core::fmt;
use rustc_mir_dataflow::{fmt::DebugWithContext, lattice::JoinSemiLattice};

#[derive(Eq, PartialEq, Clone, Debug, Default)]
pub(crate) struct RWCountDomain {
    reads: Size,
    writes: Size,
}

impl RWCountDomain {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_reads(&mut self, size: Size) {
        self.reads = self.reads.clone() + size;
    }

    pub fn add_writes(&mut self, size: Size) {
        self.writes = self.writes.clone() + size;
    }
}

impl JoinSemiLattice for RWCountDomain {
    fn join(&mut self, other: &Self) -> bool {
        if other.reads.is_zero() && other.writes.is_zero()
            || self.reads == other.reads && self.writes == other.writes
        {
            false
        } else {
            self.reads = self.reads.max(&other.reads);
            self.writes = self.writes.max(&other.writes);
            true
        }
    }
}

impl<C> DebugWithContext<C> for RWCountDomain {}

impl fmt::Display for RWCountDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "reads: {}\nwrites: {}\n", self.reads, self.writes)
    }
}
