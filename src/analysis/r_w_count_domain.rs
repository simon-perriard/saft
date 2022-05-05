use crate::size_language::Size;
use core::fmt;
use rpds::HashTrieMap;
use rustc_middle::mir::Local;
use rustc_mir_dataflow::{fmt::DebugWithContext, lattice::JoinSemiLattice};
use rustc_target::abi::VariantIdx;

#[derive(Eq, PartialEq, Clone, Debug)]
pub(crate) struct RWCountDomain {
    reads: Size,
    writes: Size,
    events: Size,
    pub bb_set_discriminant: HashTrieMap<Local, VariantIdx>,
}

impl Default for RWCountDomain {
    fn default() -> Self {
        RWCountDomain {
            reads: Size::default(),
            writes: Size::default(),
            events: Size::default(),
            bb_set_discriminant: HashTrieMap::new(),
        }
    }
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

    pub fn add_events(&mut self, size: Size) {
        self.events = self.events.clone() + size;
    }

    pub fn inter_join(&mut self, other: &Self) {
        self.reads = self.reads.clone() + other.reads.clone();
        self.writes = self.writes.clone() + other.writes.clone();
        self.events = self.events.clone() + other.events.clone();
    }

    pub fn reset_bb_discriminants(&mut self) {
        self.bb_set_discriminant = HashTrieMap::new();
    }
}

impl JoinSemiLattice for RWCountDomain {
    fn join(&mut self, other: &Self) -> bool {
        self.bb_set_discriminant = other.bb_set_discriminant.clone();

        if other.reads.is_zero() && other.writes.is_zero() && other.events.is_zero()
            || self.reads == other.reads
                && self.writes == other.writes
                && self.events == other.events
        {
            false
        } else {
            self.reads = self.reads.max(&other.reads);
            self.writes = self.writes.max(&other.writes);
            self.events = self.events.max(&other.events);
            true
        }
    }
}

impl<C> DebugWithContext<C> for RWCountDomain {}

impl fmt::Display for RWCountDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "reads: {}\nwrites: {}\nevents: {}\n",
            self.reads, self.writes, self.events
        )
    }
}
