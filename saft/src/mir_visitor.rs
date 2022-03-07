use crate::extrinsic_visitor::ExtrinsicVisitor;
use rustc_middle::mir::visit::Visitor;
use rustc_middle::mir::{Terminator,TerminatorKind};
use rustc_middle::mir::{Place,Operand};
use rustc_middle::mir::visit::PlaceContext;
use rustc_middle::mir::{Location,BasicBlock};
use rustc_middle::ty::TyKind;
use rustc_span::def_id::DefId;

pub struct MirVisitor<'tcx, 'analysis, 'body> {
    pub ev: &'body mut ExtrinsicVisitor<'tcx, 'analysis>
}

impl<'tcx, 'analysis, 'body> MirVisitor<'tcx, 'analysis, 'body> {
    pub fn new(ev: &'body mut ExtrinsicVisitor<'tcx, 'analysis>) -> MirVisitor<'tcx, 'analysis, 'body> {
        
        MirVisitor {
            ev,
        }
    }

    pub fn start_visit(&mut self) {
        self.visit_body(self.ev.mir);
    }
}

impl<'tcx, 'analysis, 'body> MirVisitor<'tcx, 'analysis, 'body> {

    fn get_callee_def_id(&mut self, func: &Operand<'tcx>) -> Option<&DefId> {
        //println!("{:?}", func);
        match func {
            Operand::Copy(Place {local, projection}) => {
                None
            },
            Operand::Move(Place {local, projection}) => {
                None
            },
            Operand::Constant(constant) => {
                match constant.ty().kind() {
                    TyKind::FnDef(def_id, _) => {
                        Some(def_id)
                    }
                     _ => None
                }
            }
        }
    }

    fn visit_call(
        &mut self,
        func: &Operand<'tcx>,
        args: &Vec<Operand<'tcx>>,
        destination: &Option<(Place<'tcx>, BasicBlock)>,
        cleanup: Option<BasicBlock>,
        from_hir_call: bool,
        fn_span: &rustc_span::Span,
        location: Location
    ) {

        let tcx = self.ev.tcx;

        if let Some(fn_def_id) = self.get_callee_def_id(func) {
            if tcx.is_mir_available(fn_def_id) {
                let called_fn_mir = tcx.optimized_mir(fn_def_id);
                println!("{:?}", called_fn_mir);
            }
        }
        //let called_fn_mir = self.ev.tcx.optimized_mir(fn_def_id);
        //println!("{:?}", func);
    }
}

impl<'tcx> Visitor<'tcx> for MirVisitor<'tcx, '_, '_> {

    fn visit_terminator(
        &mut self,
        terminator: &Terminator<'tcx>,
        location: Location
    ) {
        let Terminator { source_info, kind} = terminator;
        match kind {
            TerminatorKind::Call {
                func,
                args,
                destination,
                cleanup,
                from_hir_call,
                fn_span
            } => {
                //self.visit_source_info(source_info);
                self.visit_call(func, args, destination, *cleanup, *from_hir_call, fn_span, location);
            },
            _ => self.super_terminator(terminator, location)
        }
    }
}
