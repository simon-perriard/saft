use rpds::HashTrieSet;
use rustc_middle::mir::BasicBlock;
use rustc_mir_dataflow::{fmt::DebugWithContext, lattice::JoinSemiLattice};
use rustc_span::def_id::DefId;
use crate::pallet::Field;

#[derive(Eq, PartialEq, Clone, Debug)]
pub(crate) struct StorageCallsDomain(HashTrieSet<(BasicBlock, DefId, Field)>);

impl StorageCallsDomain {
    pub fn new() -> Self {
        StorageCallsDomain(HashTrieSet::new())
    }

    pub fn add(&mut self, bb: BasicBlock, def_id: DefId, field: Field) {
        self.0.insert_mut((bb, def_id, field));
    }

    pub fn storage_accesses(&self) -> Vec<(BasicBlock, DefId, Field)> {
        self.0.iter().map(|(bb, def_id, field)| (bb.clone(), def_id.clone(), field.clone())).collect::<Vec<(BasicBlock, DefId, Field)>>()
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
