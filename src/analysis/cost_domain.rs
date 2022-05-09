use crate::size_language::Size;
use core::fmt;
use rpds::HashTrieMap;
use rustc_middle::mir::Local;
use rustc_mir_dataflow::{fmt::DebugWithContext, lattice::JoinSemiLattice};
use rustc_target::abi::VariantIdx;

use crate::time_language::Time;

#[derive(Eq, PartialEq, Clone, Debug)]
pub(crate) struct CostDomain {
    reads: Size,
    writes: Size,
    events: Size,
    time: Time,
    pub bb_set_discriminant: HashTrieMap<Local, VariantIdx>,
}

impl Default for CostDomain {
    fn default() -> Self {
        CostDomain {
            reads: Size::default(),
            writes: Size::default(),
            events: Size::default(),
            time: Time::default(),
            bb_set_discriminant: HashTrieMap::new(),
        }
    }
}

impl CostDomain {
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
    pub fn add_time(&mut self, time: Time) {
        self.time = self.time.clone() + time;
    }

    pub fn inter_join(&mut self, other: &Self) {
        self.reads = self.reads.clone() + other.reads.clone();
        self.writes = self.writes.clone() + other.writes.clone();
        self.events = self.events.clone() + other.events.clone();
        self.time = self.time.clone() + other.time.clone();
    }

    pub fn reset_bb_discriminants(&mut self) {
        self.bb_set_discriminant = HashTrieMap::new();
    }
}

impl JoinSemiLattice for CostDomain {
    fn join(&mut self, other: &Self) -> bool {
        self.bb_set_discriminant = other.bb_set_discriminant.clone();

        if other.reads.is_zero()
            && other.writes.is_zero()
            && other.events.is_zero()
            && other.time.is_zero()
            || self.reads == other.reads
                && self.writes == other.writes
                && self.events == other.events
                && self.time == other.time
        {
            false
        } else {
            self.reads = self.reads.max(&other.reads);
            self.writes = self.writes.max(&other.writes);
            self.events = self.events.max(&other.events);
            self.time = self.time.max(&other.time);
            true
        }
    }
}

impl<C> DebugWithContext<C> for CostDomain {}

impl fmt::Display for CostDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "reads: {}\nwrites: {}\nevents: {}\ntime: {}\n",
            self.reads, self.writes, self.events, self.time
        )
    }
}
