use crate::extrinsic_visitor::ExtrinsicVisitor;
use rustc_middle::mir::BasicBlock;
use rustc_middle::mir::{Statement, StatementKind};
use rustc_middle::mir::{Terminator, TerminatorKind};
use rpds::HashTrieSet;

pub struct MirBodyVisitor<'tcx, 'analysis, 'body> {
    pub ev: &'body mut ExtrinsicVisitor<'tcx, 'analysis>,
    pub block_indices: Vec<BasicBlock>,
    already_visited: HashTrieSet<BasicBlock>
}

impl<'tcx, 'analysis, 'body> MirBodyVisitor<'tcx, 'analysis, 'body> {
    pub fn new(ev: &'body mut ExtrinsicVisitor<'tcx, 'analysis>) -> MirBodyVisitor<'tcx, 'analysis, 'body> {
        let block_indices  = ev.mir.basic_blocks().indices().collect();
        
        MirBodyVisitor {
            ev,
            block_indices,
            already_visited: HashTrieSet::new(),
        }
    }
}

impl<'tcx, 'analysis, 'body> MirBodyVisitor<'tcx, 'analysis, 'body> {

    pub fn start_analysis(&mut self) {
        self.visit_basic_block(self.block_indices[0]);
    }

    fn visit_basic_block(&mut self, bb: BasicBlock) {

        self.already_visited.insert_mut(bb);

        let bb_data = &self.ev.mir.basic_blocks()[bb];

        if bb_data.is_cleanup {
            return;
        }

        for statement in &bb_data.statements {
            self.visit_statement(&statement);
        }

        if let Some(terminator) = &bb_data.terminator {
            self.visit_terminator(&terminator);

        }
    }

    fn visit_statement(&mut self, statement: &Statement<'tcx>) {

        let Statement{source_info, kind} = statement;

        match kind {
            StatementKind::Assign(box (place, rvalue)) => {
            },
            StatementKind::FakeRead(..) => {

            },
            StatementKind::SetDiscriminant{box place, variant_index} => {

            },
            StatementKind::StorageLive(..) => {

            },
            StatementKind::StorageDead(..) => {

            },
            StatementKind::Retag(retag_kind, box place) => {

            },
            StatementKind::AscribeUserType(box (place, user_type_projection), variance) => {

            },
            StatementKind::Coverage(box coverage) => {

            },
            StatementKind::CopyNonOverlapping(box copy_non_overlapping) => {

            },
            StatementKind::Nop => {

            }

        }
    }

    fn visit_rvalue(&mut self) {

    }

    fn visit_terminator(&mut self, terminator: &Terminator<'tcx>) {
        let Terminator{source_info, kind} = terminator;

        match kind {
            TerminatorKind::Goto {
                target } => {
                self.visit_basic_block(*target)
            },
            TerminatorKind::SwitchInt {
                discr,
                switch_ty,
                targets } => {
                    for target in targets.all_targets() {
                        self.visit_basic_block(*target);
                    }
                },
            TerminatorKind::Resume => {},
            TerminatorKind::Abort => {},
            TerminatorKind::Return => {},
            TerminatorKind::Unreachable => {},
            TerminatorKind::Drop {
                place,
                target,
                unwind
            } =>  {
                self.visit_basic_block(*target);
                if let Some(unwind_target) = unwind {
                    self.visit_basic_block(*unwind_target);
                }
            },
            TerminatorKind::DropAndReplace {
                place,
                value,
                target,
                unwind
            } => {
                self.visit_basic_block(*target);
                if let Some(unwind_target) = unwind {
                    self.visit_basic_block(*unwind_target);
                }
            },
            TerminatorKind::Call {
                func,
                args,
                destination,
                cleanup,
                from_hir_call,
                fn_span
            } => {
                self.resolve_call();
            },
            TerminatorKind::Assert {
                cond,
                expected,
                msg,
                target,
                cleanup
            } => {},
            TerminatorKind::Yield {
                value,
                resume,
                resume_arg,
                drop
            } => {},
            TerminatorKind::GeneratorDrop => {},
            TerminatorKind::FalseEdge {
                real_target,
                imaginary_target
            } => {},
            TerminatorKind::FalseUnwind {
                real_target,
                unwind
            } => {},
            TerminatorKind::InlineAsm {
                template,
                operands,
                options,
                line_spans,
                destination,
                cleanup
            } => {}
        }
    }

    fn resolve_call(&self) {

    }
}