use rpds::HashTrieSet;
use rustc_middle::mir::BasicBlock;
use rustc_mir_dataflow::{fmt::DebugWithContext, lattice::JoinSemiLattice};

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct StorageCallsDomain(HashTrieSet<BasicBlock>);

impl StorageCallsDomain {
    pub fn new() -> Self {
        StorageCallsDomain(HashTrieSet::new())
    }

    pub fn add(&mut self, bb: BasicBlock) {
        self.0.insert_mut(bb);
    }

    pub fn storage_accesses(&self) -> Vec<BasicBlock> {
        self.0.iter().map(|bb| bb.clone()).collect::<Vec<BasicBlock>>()
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
