use rustc_mir_dataflow::{fmt::DebugWithContext, lattice::JoinSemiLattice};

#[derive(Eq, PartialEq, Clone, Debug, Default)]
pub(crate) struct RWCountDomain {
    reads: u32,
    writes: u32,
}

impl RWCountDomain {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_reads(&mut self, n: u32) {
        self.reads += n;
    }

    pub fn add_writes(&mut self, n: u32) {
        self.writes += n;
    }
}

impl JoinSemiLattice for RWCountDomain {
    fn join(&mut self, other: &Self) -> bool {
        let mut res = false;

        if other.reads > self.reads {
            res = true;
            self.reads += other.reads;
        }

        if other.writes > self.writes {
            res = true;
            self.writes += other.writes;
        }

        res
    }
}

impl<C> DebugWithContext<C> for RWCountDomain {}