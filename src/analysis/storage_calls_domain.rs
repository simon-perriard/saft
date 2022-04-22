use crate::pallet::Field;
use rpds::HashTrieSet;
use rustc_middle::mir::BasicBlock;
use rustc_mir_dataflow::{fmt::DebugWithContext, lattice::JoinSemiLattice};
use rustc_span::def_id::DefId;

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub(crate) enum AccessType {
    Direct(DefId, Field),
    Indirect(DefId),
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub(crate) struct StorageCallsDomain(HashTrieSet<(BasicBlock, AccessType)>);

impl StorageCallsDomain {
    pub fn new() -> Self {
        StorageCallsDomain(HashTrieSet::new())
    }

    pub fn add(&mut self, bb: BasicBlock, access: AccessType) {
        self.0.insert_mut((bb, access));
    }

    pub fn storage_accesses(&self) -> Vec<(BasicBlock, AccessType)> {
        self.0
            .iter()
            .map(|(bb, access)| (bb.clone(), access.clone()))
            .collect::<Vec<(BasicBlock, AccessType)>>()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl JoinSemiLattice for StorageCallsDomain {
    fn join(&mut self, other: &Self) -> bool {
        let mut res = false;
        for elem in other.0.iter() {
            if !self.0.contains(elem) {
                res = true;
                self.0.insert_mut(elem.clone());
            }
        }
        res
    }
}

impl<C> DebugWithContext<C> for StorageCallsDomain {}
