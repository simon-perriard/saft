use rpds::HashTrieSet;
use rustc_span::def_id::DefId;
use rustc_mir_dataflow::{lattice::JoinSemiLattice, fmt::DebugWithContext};
use rustc_middle::mir::BasicBlock;

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct StorageCallsDomain(HashTrieSet<(DefId, BasicBlock)>);

impl StorageCallsDomain {
    pub fn new() -> Self {
        StorageCallsDomain(HashTrieSet::new())
    }

    pub fn add(&mut self, def_id: DefId, bb: BasicBlock) {
        self.0.insert_mut((def_id, bb));
    }
}

impl JoinSemiLattice for StorageCallsDomain {
    fn join(&mut self, other: &Self) -> bool {
        let mut res = false;
        for elem in other.0.iter() {
            if !self.0.contains(elem) {
                res = true;
                self.0.insert_mut(*elem);
            }
        }
        res
    }
}

impl<C> DebugWithContext<C> for StorageCallsDomain {}