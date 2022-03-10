use crate::extrinsic_visitor::ExtrinsicVisitor;
use crate::storage_actions::apply_r_w;
use rustc_middle::mir::visit::*;
use rustc_middle::mir::*;
use rustc_middle::ty::{TyKind,Const};
use rpds::{HashTrieSet,HashTrieMap};
use rustc_hir::def_id::DefId;

pub struct Context {
    pub reads: u32,
    pub writes: u32
    //pub environment
}

impl Context {
    pub fn new() -> Context {
        Context {
            reads: 0,
            writes: 0
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}


pub struct MirVisitor<'tcx, 'extrinsic, 'analysis> {
    pub ev: &'analysis ExtrinsicVisitor<'tcx, 'extrinsic>,
    already_visited_bodies: HashTrieSet<DefId>,
    pub bodies_weights: HashTrieMap<DefId,(u32, u32)>,
    already_visited_blocks: HashTrieMap<DefId, HashTrieSet<BasicBlock>>,
    pub basic_blocks_weights: HashTrieMap<DefId, HashTrieMap<BasicBlock, (u32, u32)>>,
    pub context: Context
}

impl<'tcx, 'extrinsic, 'analysis> MirVisitor<'tcx, 'extrinsic, 'analysis> {
    pub fn new(ev: &'analysis ExtrinsicVisitor<'tcx, 'extrinsic>) -> MirVisitor<'tcx, 'extrinsic, 'analysis> {
        
        MirVisitor {
            ev,
            already_visited_bodies: HashTrieSet::new(),
            bodies_weights: HashTrieMap::new(),
            already_visited_blocks: HashTrieMap::new(),
            basic_blocks_weights: HashTrieMap::new(),
            context: Context::new()
        }
    }

    pub fn start_visit(&mut self) {

        let ev = self.ev;

        let mut top_body = MirBodyVisitor {
            mv: self,
            def_id: &ev.def_id,
            body: ev.mir,
            current_basic_block: None,
            current_bb_context: Context::new()
        };

        top_body.start_visit();
    }

}

pub struct MirBodyVisitor<'tcx, 'extrinsic, 'analysis, 'body> {
    pub mv: &'body mut MirVisitor<'tcx, 'extrinsic, 'analysis>,
    pub def_id: &'body DefId,
    pub body: &'body Body<'extrinsic>,
    current_basic_block: Option<BasicBlock>,
    pub current_bb_context: Context
}

impl<'tcx, 'extrinsic, 'analysis, 'body> MirBodyVisitor<'tcx, 'extrinsic, 'analysis, 'body> {
    pub fn start_visit(&mut self) {
        
        self.visit_body(self.body);
        
    }

    fn close_bb_analysis(&mut self) {

        // Aggregate context returned by analyzed bb
        let Context{reads, writes} = self.current_bb_context;
        self.mv.basic_blocks_weights.get_mut(self.def_id).unwrap().insert_mut(self.current_basic_block.unwrap(), (reads, writes));

        // Reset context
        self.current_bb_context = Context::default();
    }

    fn close_body_analysis(&mut self) {
        // Aggregate context returned by the analyzed body (function)
        // for now only adding read and writes in a dumb way
        // and assuming no loop or recursion
        let mut total_reads = 0;
        let mut total_writes = 0;

        for bb in self.body.basic_blocks().indices() {
            let (r, w) = self.mv.basic_blocks_weights.get(self.def_id).unwrap().get(&bb).unwrap().to_owned();
            total_reads += r;
            total_writes += w;
        }

        self.mv.bodies_weights.insert_mut(*self.def_id, (total_reads, total_writes));
    }
}


impl<'body> Visitor<'body> for MirBodyVisitor<'_, '_, '_, 'body> {

    fn visit_body(
        &mut self,
        body: &Body<'body>
    ) {
        if !self.mv.already_visited_blocks.contains_key(self.def_id) {
            // Initialize set of visited blocks for the current body
            self.mv.already_visited_blocks.insert_mut(*self.def_id, HashTrieSet::new());
            // Initialize map of basic blocks weights for every block of the current body
            self.mv.basic_blocks_weights.insert_mut(*self.def_id, HashTrieMap::new());
            self.super_body(body);

            self.close_body_analysis();
        }
        // no visit needed if already visited
    }   

    fn visit_basic_block_data(
        &mut self,
        block: BasicBlock,
        data: &BasicBlockData<'body>) {

        // Set fresh context for the new bb
        self.current_basic_block = Some(block);
        self.current_bb_context = Context::default();

        if !self.mv.already_visited_blocks.get(self.def_id).unwrap().contains(&block) {
            // bb not visited yet
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

            // If body already visited, just add weights
            // otherwise, analyse underlying
            if self.mv.already_visited_bodies.contains(def_id) {

                let (reads, writes) = self.mv.bodies_weights.get(def_id).unwrap();
                
                self.current_bb_context.reads += reads;
                self.current_bb_context.writes += writes;
            } else if tcx.is_mir_available(def_id) {
                let called_fn_mir = tcx.optimized_mir(def_id);
                
                // Continue analysis on called functions
                let mut callee_body = MirBodyVisitor {
                    mv: self.mv,
                    def_id,
                    body: called_fn_mir,
                    current_basic_block: None,
                    current_bb_context: Context::default(),
                };

                callee_body.start_visit();

                // After underlying function has been analyzed, update bb context and weights
                let (reads, writes) = self.mv.bodies_weights.get(def_id).unwrap();

                self.current_bb_context.reads += reads;
                self.current_bb_context.writes += writes;
            } else {
                apply_r_w(tcx, *def_id, &mut self.current_bb_context);
            }
        }
    }

    fn visit_terminator(
        &mut self,
        terminator: &Terminator<'body>,
        location: Location) {

        self.super_terminator(terminator, location);
        self.close_bb_analysis();
    }
}










