use crate::analysis_utils::get_fn_name;
use crate::extrinsic_visitor::ExtrinsicVisitor;
use rustc_middle::mir::visit::Visitor;
use rustc_middle::mir::Location;
use rustc_middle::ty::{TyKind,Const};
use rustc_middle::mir::{Body,BasicBlock,BasicBlockData};
use rpds::{HashTrieSet,HashTrieMap};
use rustc_hir::def_id::DefId;


pub struct MirVisitor<'tcx, 'extrinsic, 'analysis> {
    pub ev: &'analysis ExtrinsicVisitor<'tcx, 'extrinsic>,
    already_visited_blocks: HashTrieMap<DefId, HashTrieSet<BasicBlock>>
    // add something about the environment for the analysis
}

impl<'tcx, 'extrinsic, 'analysis> MirVisitor<'tcx, 'extrinsic, 'analysis> {
    pub fn new(ev: &'analysis ExtrinsicVisitor<'tcx, 'extrinsic>) -> MirVisitor<'tcx, 'extrinsic, 'analysis> {
        
        MirVisitor {
            ev,
            already_visited_blocks: HashTrieMap::new()
        }
    }

    pub fn start_visit(&mut self) {

        let ev = self.ev;

        let mut top_body = MirBodyVisitor {
            mv: self,
            def_id: &ev.def_id,
            body: ev.mir,
        };

        top_body.start_visit();
    }
}

pub struct MirBodyVisitor<'tcx, 'extrinsic, 'analysis, 'body> {
    pub mv: &'body mut MirVisitor<'tcx, 'extrinsic, 'analysis>,
    pub def_id: &'body DefId,
    pub body: &'body Body<'extrinsic>,
}

impl<'tcx, 'extrinsic, 'analysis, 'body> MirBodyVisitor<'tcx, 'extrinsic, 'analysis, 'body> {
    pub fn start_visit(&mut self) {

        self.visit_body(self.body);
        
    }
}


impl<'body> Visitor<'body> for MirBodyVisitor<'_, '_, '_, 'body> {

    fn visit_body(
        &mut self,
        body: &Body<'body>
    ) {
        if !self.mv.already_visited_blocks.contains_key(self.def_id) {
            self.mv.already_visited_blocks.insert_mut(*self.def_id, HashTrieSet::new());
        }
        self.super_body(body);
    }

    fn visit_basic_block_data(
        &mut self,
        block: BasicBlock,
        data: &BasicBlockData<'body>) {
        
        println!("{}", get_fn_name(self.mv.ev.tcx, *self.def_id));

        if self.mv.already_visited_blocks.get(self.def_id).unwrap().contains(&block) {
            // defId already visited and basic block already visited.
            println!("ALREADY VISITED");
        } else {
            // defId already visited but not basic block
            self.mv.already_visited_blocks.get_mut(self.def_id).unwrap().insert_mut(block);
            self.super_basic_block_data(block, data);
        }
    }

    fn visit_const (
        &mut self,
        cst: Const<'body>,
        _location: Location
    ) {
        let ty = cst.ty();
        let tcx = self.mv.ev.tcx;

        // continue analysis on function calls
        let opt_def_id = match ty.kind() {
            TyKind::FnDef(def_id, _) => {
                Some(def_id)
            },
            TyKind::Closure(def_id, _) => {
                Some(def_id)
            },
            _ => {
                None
            }
        };

        if let Some(def_id) = opt_def_id {
            if tcx.is_mir_available(def_id) {
                let called_fn_mir = tcx.optimized_mir(def_id);
                
                // Continue analysis on called functions
                let mut callee_body = MirBodyVisitor {
                    mv: self.mv,
                    def_id,
                    body: called_fn_mir,
                };

                callee_body.start_visit();
            }
        }
        
    }
}
