use crate::dispatchable_visitor::DispatchableVisitor;
use crate::storage_actions::apply_r_w;
use crate::weights::Weights;
use rpds::{HashTrieMap, HashTrieSet};
use rustc_hir::def_id::DefId;
use rustc_index::bit_set::BitSet;
use rustc_middle::mir::visit::*;
use rustc_middle::mir::*;
use rustc_middle::ty::{Const, TyKind};
use rustc_middle::ty::*;

pub struct Context {
    pub weights: Weights,
}

impl Context {
    pub fn new() -> Context {
        Context {
            weights: Weights::default(),
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

pub struct MirVisitor<'tcx, 'pallet, 'analysis> {
    pub ev: &'analysis DispatchableVisitor<'tcx, 'pallet>,
    already_visited_bodies: HashTrieSet<DefId>,
    pub bodies_weights: HashTrieMap<DefId, Weights>,
    already_visited_blocks: HashTrieMap<DefId, HashTrieSet<BasicBlock>>,
    pub basic_blocks_weights: HashTrieMap<DefId, HashTrieMap<BasicBlock, Weights>>,
    pub context: Context,
}

impl<'tcx, 'pallet, 'analysis> MirVisitor<'tcx, 'pallet, 'analysis> {
    pub fn new(
        ev: &'analysis DispatchableVisitor<'tcx, 'pallet>,
    ) -> MirVisitor<'tcx, 'pallet, 'analysis> {
        MirVisitor {
            ev,
            already_visited_bodies: HashTrieSet::new(),
            bodies_weights: HashTrieMap::new(),
            already_visited_blocks: HashTrieMap::new(),
            basic_blocks_weights: HashTrieMap::new(),
            context: Context::new(),
        }
    }

    pub fn start_visit(&mut self) {
        let ev = self.ev;

        let body = ev.mir;
        let bitset_domain = body.basic_blocks().len();

        let mut top_body = MirBodyVisitor {
            mv: self,
            def_id: &ev.def_id,
            body,
            current_basic_block: None,
            current_bb_context: Context::new(),
            is_bb_being_recursively_visited: BitSet::new_empty(bitset_domain),
        };

        top_body.start_visit();
    }
}

pub struct MirBodyVisitor<'tcx, 'pallet, 'analysis, 'body> {
    pub mv: &'body mut MirVisitor<'tcx, 'pallet, 'analysis>,
    pub def_id: &'body DefId,
    pub body: &'tcx Body<'tcx>,
    current_basic_block: Option<BasicBlock>,
    pub current_bb_context: Context,
    pub is_bb_being_recursively_visited: BitSet<BasicBlock>,
}

impl<'tcx, 'pallet, 'analysis, 'body> MirBodyVisitor<'tcx, 'pallet, 'analysis, 'body> {
    pub fn start_visit(&mut self) {
        self.visit_body(self.body);
    }

    fn close_bb_analysis(&mut self) {
        // Aggregate context returned by analyzed bb
        self.mv
            .basic_blocks_weights
            .get_mut(self.def_id)
            .unwrap()
            .insert_mut(
                self.current_basic_block.unwrap(),
                self.current_bb_context.weights,
            );

        // Reset context
        self.current_bb_context = Context::default();
    }

    fn close_body_analysis(&mut self) {
        let body_weights_agg = self.traverse_and_aggregate_weights();

        self.mv
            .bodies_weights
            .insert_mut(*self.def_id, body_weights_agg);
    }
}

impl<'tcx> Visitor<'tcx> for MirBodyVisitor<'tcx, '_, '_, '_> {
    fn visit_body(&mut self, body: &Body<'tcx>) {
        if !self.mv.already_visited_blocks.contains_key(self.def_id) {
            // Initialize set of visited blocks for the current body
            self.mv
                .already_visited_blocks
                .insert_mut(*self.def_id, HashTrieSet::new());
            // Initialize map of basic blocks weights for every block of the current body
            self.mv
                .basic_blocks_weights
                .insert_mut(*self.def_id, HashTrieMap::new());
            self.super_body(body);

            self.close_body_analysis();
        }
        // no visit needed if already visited
    }

    fn visit_basic_block_data(&mut self, block: BasicBlock, data: &BasicBlockData<'tcx>) {
        // Set fresh context for the new bb
        self.current_basic_block = Some(block);
        self.current_bb_context = Context::default();

        if !self
            .mv
            .already_visited_blocks
            .get(self.def_id)
            .unwrap()
            .contains(&block)
        {
            // bb not visited yet
            self.mv
                .already_visited_blocks
                .get_mut(self.def_id)
                .unwrap()
                .insert_mut(block);
            self.super_basic_block_data(block, data);
        }
    }

    fn visit_const(&mut self, cst: Const<'tcx>, _location: Location) {
        let ty = cst.ty();
        let tcx = self.mv.ev.tcx;

        // continue analysis on function calls
        let opt_def_id = match ty.kind() {
            TyKind::FnDef(def_id, _) => Some(def_id),
            TyKind::Closure(def_id, _) => {
                //tcx.def_path_str(*def_id);
                Some(def_id)
            }
            _ => None,
        };

        if let Some(def_id) = opt_def_id {
            // If body already visited, just add weights
            // otherwise, analyse underlying
            if self.mv.already_visited_bodies.contains(def_id) {
                let known_body_weights = self.mv.bodies_weights.get(def_id).unwrap().to_owned();
                self.current_bb_context.weights += known_body_weights;
            } else if tcx.is_mir_available(def_id) {
                let called_fn_mir = tcx.optimized_mir(def_id);

                let bitset_domain = called_fn_mir.basic_blocks().len();

                // Continue analysis on called functions
                let mut callee_body = MirBodyVisitor {
                    mv: self.mv,
                    def_id,
                    body: called_fn_mir,
                    current_basic_block: None,
                    current_bb_context: Context::default(),
                    is_bb_being_recursively_visited: BitSet::new_empty(bitset_domain),
                };

                callee_body.start_visit();

                // After underlying function has been analyzed, update bb context and weights
                let weights = self.mv.bodies_weights.get(def_id);

                if let Some(weights) = weights {
                    self.current_bb_context.weights += *weights;
                } else {
                    //println!("RECURSION DETECTED, RESULT WILL PROBABLY BE WRONG");
                }
            } else {
                apply_r_w(tcx, *def_id, &mut self.current_bb_context);
            }
        }
    }

    fn visit_terminator(&mut self, terminator: &Terminator<'tcx>, location: Location) {

        if let TerminatorKind::Call { func, .. } = &terminator.kind
        && let Operand::Constant(c) = func
        && let TyKind::FnDef(def_id, substs) = c.ty().kind()
        {
            if self.mv.ev.tcx.def_path_str(*def_id).starts_with("frame_support::pallet_prelude::Storage"){
                let pallet = self.mv.ev.pallet;
                let tcx = self.mv.ev.tcx;
                let key = tcx.def_key(*def_id);
                let parent_def_id = DefId { index: key.parent.unwrap(), ..*def_id };

                if let TyKind::Adt(adt_def_data, _) = tcx.type_of(parent_def_id).kind() {
                    let reconstructed_ty = tcx.mk_adt(adt_def_data, substs);
                    //println!("{:?}", reconstructed_ty);
                    for ty in pallet.fields.keys().map(|field_def_id| tcx.type_of(field_def_id)) {
                        if ty == reconstructed_ty {
                            println!("");
                        }
                    }
                }

                
                //let parent_ty = tcx.type_of();

                //let local_def_id = def_id.expect_local();
                
                //let body_owner = tcx.hir().body_owned_by(tcx.hir().local_def_id_to_hir_id());
                //tcx.hir().body_owned_by()

                /*for ty in pallet.fields.keys().map(|field_def_id| tcx.type_of(field_def_id)) {
                    println!("{:?}", ty);
                }*/

                //let generics = tcx.generics_of(def_id);
                //println!("{},{}", generics.parent_count, substs.len());
                //let parent_substs = &substs[..generics.parent_count.min(substs.len())];
                //println!("{:?}", parent_substs);

                /*let trait_ref = rustc_middle::ty::TraitRef::new(
                    parent_def_id,
                    tcx.intern_substs(parent_substs)
                );*/

                //println!("{:?}", trait_ref);

                //println!();
                //println!();

                // reconstruct type from what we have
                //let adt_def_data = rustc_middle::ty::adt::AdtDefData::new();
                //let ty = tcx.mk_adt(adt_def_data, substs);
            }
        }

        self.super_terminator(terminator, location);
        self.close_bb_analysis();
    }
}

impl<'tcx, 'pallet, 'analysis, 'body> MirBodyVisitor<'tcx, 'pallet, 'analysis, 'body> {
    fn traverse_and_aggregate_weights(&mut self) -> Weights {
        self.visit_basic_block_data_recursive(&START_BLOCK)
    }

    fn visit_basic_block_data_recursive(&mut self, block: &BasicBlock) -> Weights {
        let data = &self.body.basic_blocks()[*block];
        let BasicBlockData { terminator, .. } = data;

        //Retrieve weights from successors through the terminator and add them to the current bb weights
        let bb_weights = self
            .mv
            .basic_blocks_weights
            .get(self.def_id)
            .unwrap()
            .get(block)
            .unwrap()
            .to_owned();

        if let Some(terminator) = terminator {
            if self.is_bb_being_recursively_visited.contains(*block) {
                //println!("LOOP DETECTED, RESULT WILL PROBABLY BE WRONG");
                bb_weights
            } else {
                self.is_bb_being_recursively_visited.insert(*block);
                let max_succ_weights = self.visit_terminator_recursive(terminator);
                self.is_bb_being_recursively_visited.remove(*block);
                bb_weights + max_succ_weights
            }
        } else {
            bb_weights
        }
    }

    fn visit_terminator_recursive(&mut self, terminator: &Terminator<'body>) -> Weights {
        let Terminator { kind, .. } = terminator;

        match kind {
            TerminatorKind::Goto { target } => self.visit_basic_block_data_recursive(target),
            TerminatorKind::SwitchInt { targets, .. } => {
                let mut max_weights = Weights::default();
                // Iterate through successors and return max
                for target in targets.all_targets() {
                    let target_weights = self.visit_basic_block_data_recursive(target);

                    max_weights = Weights::max(max_weights, target_weights);
                }
                max_weights
            }
            TerminatorKind::Drop { target, unwind, .. }
            | TerminatorKind::DropAndReplace { target, unwind, .. } => {
                let unwind_weights = if let Some(unwind) = unwind {
                    self.visit_basic_block_data_recursive(unwind)
                } else {
                    Weights::default()
                };

                let target_weights = self.visit_basic_block_data_recursive(target);

                if target_weights < unwind_weights {
                    unwind_weights
                } else {
                    target_weights
                }
            }
            TerminatorKind::Call {
                destination,
                cleanup,
                ..
            } => {
                let dest_weights = if let Some((_, destination)) = destination {
                    self.visit_basic_block_data_recursive(destination)
                } else {
                    Weights::default()
                };

                let cleanup_weights = if let Some(cleanup) = cleanup {
                    self.visit_basic_block_data_recursive(cleanup)
                } else {
                    Weights::default()
                };

                if dest_weights < cleanup_weights {
                    cleanup_weights
                } else {
                    dest_weights
                }
            }
            _ => Weights::default(),
        }
    }
}
