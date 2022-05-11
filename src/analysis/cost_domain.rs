use crate::cost_language::Cost;
use core::fmt;
use rpds::HashTrieMap;
use rustc_middle::mir::Local;
use rustc_mir_dataflow::{fmt::DebugWithContext, lattice::JoinSemiLattice};
use rustc_target::abi::VariantIdx;

#[derive(Eq, PartialEq, Clone, Debug)]
pub(crate) struct CostDomain {
    bytes_read: Cost,
    bytes_written: Cost,
    bytes_deposited: Cost,
    steps_executed: Cost,
    pub bb_set_discriminant: HashTrieMap<Local, VariantIdx>,
}

impl Default for CostDomain {
    fn default() -> Self {
        CostDomain {
            bytes_read: Cost::default(),
            bytes_written: Cost::default(),
            bytes_deposited: Cost::default(),
            steps_executed: Cost::default(),
            bb_set_discriminant: HashTrieMap::new(),
        }
    }
}

impl CostDomain {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_reads(&mut self, size: Cost) {
        self.bytes_read = self.bytes_read.clone() + size;
    }

    pub fn add_writes(&mut self, size: Cost) {
        self.bytes_written = self.bytes_written.clone() + size;
    }

    pub fn add_events(&mut self, size: Cost) {
        self.bytes_deposited = self.bytes_deposited.clone() + size;
    }
    pub fn add_steps(&mut self, steps: Cost) {
        self.steps_executed = self.steps_executed.clone() + steps;
    }

    pub fn inter_join(&mut self, other: &Self) {
        self.bytes_read = self.bytes_read.clone() + other.bytes_read.clone();
        self.bytes_written = self.bytes_written.clone() + other.bytes_written.clone();
        self.bytes_deposited = self.bytes_deposited.clone() + other.bytes_deposited.clone();
        self.steps_executed = self.steps_executed.clone() + other.steps_executed.clone();
    }

    pub fn reset_bb_discriminants(&mut self) {
        self.bb_set_discriminant = HashTrieMap::new();
    }
}

impl JoinSemiLattice for CostDomain {
    fn join(&mut self, other: &Self) -> bool {
        self.bb_set_discriminant = other.bb_set_discriminant.clone();

        if other.bytes_read.is_zero()
            && other.bytes_written.is_zero()
            && other.bytes_deposited.is_zero()
            && other.steps_executed.is_zero()
            || self.bytes_read == other.bytes_read
                && self.bytes_written == other.bytes_written
                && self.bytes_deposited == other.bytes_deposited
                && self.steps_executed == other.steps_executed
        {
            false
        } else {
            self.bytes_read = self.bytes_read.max(&other.bytes_read);
            self.bytes_written = self.bytes_written.max(&other.bytes_written);
            self.bytes_deposited = self.bytes_deposited.max(&other.bytes_deposited);
            self.steps_executed = self.steps_executed.max(&other.steps_executed);
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
            self.bytes_read, self.bytes_written, self.bytes_deposited, self.steps_executed
        )
    }
}
