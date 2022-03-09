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
    //bodies_weights: HashTrieMap<DefId,(u32, u32)>,
    already_visited_blocks: HashTrieMap<DefId, HashTrieSet<BasicBlock>>,
    pub basic_blocks_weights: HashTrieMap<DefId, HashTrieMap<BasicBlock, (u32, u32)>>,
    pub context: Context
}

impl<'tcx, 'extrinsic, 'analysis> MirVisitor<'tcx, 'extrinsic, 'analysis> {
    pub fn new(ev: &'analysis ExtrinsicVisitor<'tcx, 'extrinsic>) -> MirVisitor<'tcx, 'extrinsic, 'analysis> {
        
        MirVisitor {
            ev,
            already_visited_bodies: HashTrieSet::new(),
            //bodies_weights: HashTrieMap::new(),
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
            body_context: Context::new(),
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
    pub body_context: Context,
    pub current_bb_context: Context
}

impl<'tcx, 'extrinsic, 'analysis, 'body> MirBodyVisitor<'tcx, 'extrinsic, 'analysis, 'body> {
    pub fn start_visit(&mut self) {
        self.visit_body(self.body);
    }

    fn aggregate_from_bb_context(&mut self) {
        // Aggregate context returned by analyzed bb
        let Context{reads, writes} = self.current_bb_context;
        let (old_r, old_w) = self.mv.basic_blocks_weights.get(self.def_id).unwrap().get(&self.current_basic_block.unwrap()).unwrap_or_else(|| &(0,0)).to_owned();
        self.mv.basic_blocks_weights.get_mut(self.def_id).unwrap().insert_mut(self.current_basic_block.unwrap(), (old_r+reads, old_w+writes));

        // Reset context
        self.current_bb_context = Context::default();
    }

    fn close_bb_analysis(&mut self) {
        self.aggregate_from_bb_context();

        // Push bb analysis to broader context
        //let (old_r, old_w) = self.mv.bodies_weights.get(self.def_id).unwrap_or_else(|| &(0,0)).to_owned();
        //let (r, w) = self.mv.basic_blocks_weights.get(self.def_id).unwrap().get(&self.current_basic_block.unwrap()).unwrap_or_else(|| &(0,0)).to_owned();

        //self.mv.bodies_weights.insert_mut(*self.def_id, (old_r+r, old_w+w));
    }

    fn close_body_analysis(&mut self) {

    }
}


impl<'body> Visitor<'body> for MirBodyVisitor<'_, '_, '_, 'body> {

    fn visit_body(
        &mut self,
        body: &Body<'body>
    ) {
        if !self.mv.already_visited_blocks.contains_key(self.def_id) {
            self.mv.already_visited_blocks.insert_mut(*self.def_id, HashTrieSet::new());
            self.mv.basic_blocks_weights.insert_mut(*self.def_id, HashTrieMap::new());
            self.super_body(body);
        }
        // no visit needed if already visited
    }   

    fn visit_basic_block_data(
        &mut self,
        block: BasicBlock,
        data: &BasicBlockData<'body>) {

        if let Some(_) = self.current_basic_block {
            self.close_bb_analysis();
        }

        // Set fresh context for the new bb
        self.current_basic_block = Some(block);
        self.current_bb_context = Context::default();

        if self.mv.already_visited_blocks.get(self.def_id).unwrap().contains(&block) {
            // defId already visited and basic block already visited.
            //let (r, w) = self.mv.basic_blocks_weights();

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

            // If body already visited, just add weights
            // otherwise, analyse underlying
            if self.mv.already_visited_bodies.contains(def_id) {
                // do nothing, we are only interested in bb cost
            } else if tcx.is_mir_available(def_id) {
                    let called_fn_mir = tcx.optimized_mir(def_id);
                    
                    // Continue analysis on called functions
                    let mut callee_body = MirBodyVisitor {
                        mv: self.mv,
                        def_id,
                        body: called_fn_mir,
                        current_basic_block: None,
                        body_context: Context::new(),
                        current_bb_context: Context::default(),
                    };
    
                    callee_body.start_visit();

                    // After underlying function has been analyzes, update global context and weights
                    let Context{reads, writes} = callee_body.body_context;
                    //self.mv.bodies_weights.insert_mut(*def_id, (reads, writes));

                    self.current_bb_context.reads += reads;
                    self.current_bb_context.writes += writes;
            } else {
                apply_r_w(tcx, *def_id, &mut self.current_bb_context);
            }
        }
        // SAFT aggregate here
        self.aggregate_from_bb_context();
    }

    fn visit_terminator(
        &mut self,
        terminator: &Terminator<'body>,
        location: Location) {
        let Terminator { source_info, kind } = terminator;

        self.visit_source_info(source_info);
        match kind {
            TerminatorKind::Goto { .. } |
            TerminatorKind::Resume |
            TerminatorKind::Abort |
            TerminatorKind::GeneratorDrop |
            TerminatorKind::Unreachable |
            TerminatorKind::FalseEdge { .. } |
            TerminatorKind::FalseUnwind { .. } => {
            }

            TerminatorKind::Return => {
                // `return` logically moves from the return place `_0`. Note that the place
                // cannot be changed by any visitor, though.
                let local = RETURN_PLACE;
                self.visit_local(
                    &local,
                    PlaceContext::NonMutatingUse(NonMutatingUseContext::Move),
                    location,
                );

                assert_eq!(
                    local,
                    RETURN_PLACE,
                    "`MutVisitor` tried to mutate return place of `return` terminator"
                );
            }

            TerminatorKind::SwitchInt {
                discr,
                switch_ty,
                targets: _
            } => {
                self.visit_operand(discr, location);
                // SAFT closing bb analysis
                // going to next bb after this
                self.close_bb_analysis();
                self.visit_ty(*switch_ty, TyContext::Location(location));
            }

            TerminatorKind::Drop {
                place,
                target: _,
                unwind: _,
            } => {
                self.visit_place(
                    place,
                    PlaceContext::MutatingUse(MutatingUseContext::Drop),
                    location
                );
            }

            TerminatorKind::DropAndReplace {
                place,
                value,
                target: _,
                unwind: _,
            } => {
                self.visit_place(
                    place,
                    PlaceContext::MutatingUse(MutatingUseContext::Drop),
                    location
                );
                self.visit_operand(value, location);
            }

            TerminatorKind::Call {
                func,
                args,
                destination,
                cleanup: _,
                from_hir_call: _,
                fn_span: _
            } => {
                self.visit_operand(func, location);
                for arg in args {
                    self.visit_operand(arg, location);
                }

                // SAFT closing bb analysis
                // going to next bb after this
                self.close_bb_analysis();

                if let Some((destination, _)) = destination {
                    self.visit_place(
                        destination,
                        PlaceContext::MutatingUse(MutatingUseContext::Call),
                        location
                    );
                }
            }

            TerminatorKind::Assert {
                cond,
                expected: _,
                msg,
                target: _,
                cleanup: _,
            } => {
                self.visit_operand(cond, location);
                self.visit_assert_message(msg, location);
            }

            TerminatorKind::Yield {
                value,
                resume: _,
                resume_arg,
                drop: _,
            } => {
                self.visit_operand(value, location);
                self.visit_place(
                    resume_arg,
                    PlaceContext::MutatingUse(MutatingUseContext::Yield),
                    location,
                );
            }
            // TODO find calls in asm, or disable support ?
            TerminatorKind::InlineAsm {
                template: _,
                operands,
                options: _,
                line_spans: _,
                destination: _,
                cleanup: _,
            } => {
                for op in operands {
                    match op {
                        InlineAsmOperand::In { value, .. } => {
                            self.visit_operand(value, location);
                        }
                        InlineAsmOperand::Out { place: Some(place), .. } => {
                            self.visit_place(
                                place,
                                PlaceContext::MutatingUse(MutatingUseContext::AsmOutput),
                                location,
                            );
                        }
                        InlineAsmOperand::InOut { in_value, out_place, .. } => {
                            self.visit_operand(in_value, location);
                            if let Some(out_place) = out_place {
                                self.visit_place(
                                    out_place,
                                    PlaceContext::MutatingUse(MutatingUseContext::AsmOutput),
                                    location,
                                );
                            }
                        }
                        InlineAsmOperand::Const { value }
                        | InlineAsmOperand::SymFn { value } => {
                            self.visit_constant(value, location);
                        }
                        InlineAsmOperand::Out { place: None, .. }
                        | InlineAsmOperand::SymStatic { def_id: _ } => {}
                    }
                }
            }
        }
    }
}










