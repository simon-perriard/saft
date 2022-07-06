use super::cost_domain::{ExtendedCostAnalysisDomain, TypeInfo};
use super::pallet::Pallet;
use super::specifications_v2::try_dispatch_to_specifications;
use crate::analysis::events_variants_domain::EventVariantsDomain;
use rustc_middle::mir::{
    self, traversal::*, visit::*, BasicBlock, Body, Location, Operand, Place, ProjectionElem,
    Rvalue, Statement, StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::{subst::SubstsRef, TyCtxt, TyKind};
use rustc_mir_dataflow::{Analysis, AnalysisDomain, CallReturnPlaces, Forward};
use rustc_span::def_id::DefId;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::vec;

#[derive(PartialEq, Eq)]
pub(crate) enum AnalysisState {
    Success,
    Failure,
}

impl AnalysisState {
    pub fn and(&self, rhs: &Self) -> Self {
        match self {
            AnalysisState::Success => match rhs {
                AnalysisState::Success => AnalysisState::Success,
                AnalysisState::Failure => AnalysisState::Failure,
            },
            AnalysisState::Failure => AnalysisState::Failure,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct CalleeInfo<'tcx> {
    pub location: Option<Location>,
    pub args: Vec<Operand<'tcx>>,
    pub destination: Option<Place<'tcx>>,
    pub callee_def_id: DefId,
    pub substs_ref: SubstsRef<'tcx>,
}

pub(crate) struct CostAnalysis<'tcx, 'inter> {
    tcx: TyCtxt<'tcx>,
    pallet: &'inter Pallet,
    events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
    def_id: DefId,
    caller_context_args_type_info: Vec<TypeInfo<'tcx>>,
    pub analysis_state: Rc<RefCell<AnalysisState>>,
}

impl<'tcx, 'inter, 'transformer> CostAnalysis<'tcx, 'inter> {
    pub(crate) fn new(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
        def_id: DefId,
    ) -> Self {
        Self::new_with_init(
            tcx,
            pallet,
            events_variants,
            def_id,
            Vec::new(),
            Rc::new(RefCell::new(AnalysisState::Success)),
        )
    }

    fn new_with_init(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
        def_id: DefId,
        caller_context_args_type_info: Vec<TypeInfo<'tcx>>,
        state: Rc<RefCell<AnalysisState>>,
    ) -> Self {
        CostAnalysis {
            tcx,
            pallet,
            events_variants,
            def_id,
            caller_context_args_type_info,
            analysis_state: state,
        }
    }

    pub(crate) fn transfer_function(
        &self,
        state: &'transformer mut ExtendedCostAnalysisDomain<'tcx>,
    ) -> TransferFunction<'tcx, 'inter, 'transformer> {
        TransferFunction::new(
            self.tcx,
            self.pallet,
            self.events_variants,
            self.def_id,
            state,
            self.analysis_state.clone(),
        )
    }
}

pub(crate) struct TransferFunction<'tcx, 'inter, 'transformer> {
    pub tcx: TyCtxt<'tcx>,
    pub pallet: &'inter Pallet,
    pub events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
    pub def_id: DefId,
    pub state: &'transformer mut ExtendedCostAnalysisDomain<'tcx>,
    pub analysis_state: Rc<RefCell<AnalysisState>>,
}

impl<'tcx, 'inter, 'transformer> TransferFunction<'tcx, 'inter, 'transformer> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
        def_id: DefId,
        state: &'transformer mut ExtendedCostAnalysisDomain<'tcx>,
        analysis_state: Rc<RefCell<AnalysisState>>,
    ) -> Self {
        TransferFunction {
            tcx,
            pallet,
            events_variants,
            def_id,
            state,
            analysis_state,
        }
    }
}

impl<'tcx, 'inter> AnalysisDomain<'tcx> for CostAnalysis<'tcx, 'inter> {
    type Domain = ExtendedCostAnalysisDomain<'tcx>;
    const NAME: &'static str = "CostAnalysis";

    type Direction = Forward;

    fn bottom_value(&self, body: &Body<'tcx>) -> Self::Domain {
        let mut state = ExtendedCostAnalysisDomain::new(self.tcx, body);
        state.override_with_caller_type_context(&self.caller_context_args_type_info);

        state
    }

    fn initialize_start_block(&self, _body: &Body<'tcx>, _state: &mut Self::Domain) {}
}

impl<'tcx, 'inter> Analysis<'tcx> for CostAnalysis<'tcx, 'inter> {
    fn apply_statement_effect(
        &self,
        state: &mut Self::Domain,
        statement: &Statement<'tcx>,
        location: Location,
    ) {
        if *self.analysis_state.borrow() == AnalysisState::Success {
            self.transfer_function(state)
                .visit_statement(statement, location)
        }
    }

    fn apply_terminator_effect(
        &self,
        state: &mut Self::Domain,
        terminator: &Terminator<'tcx>,
        location: Location,
    ) {
        if *self.analysis_state.borrow() == AnalysisState::Success {
            self.transfer_function(state)
                .visit_terminator(terminator, location);
        }
    }

    fn apply_call_return_effect(
        &self,
        _state: &mut Self::Domain,
        _block: BasicBlock,
        _return_place: CallReturnPlaces<'_, 'tcx>,
    ) {
        // do nothing
    }
}

impl<'visitor, 'tcx> TransferFunction<'tcx, '_, '_>
where
    Self: Visitor<'visitor>,
{
    fn visit_fn_call(&mut self, location: Location) {
        let callee_info = self.get_callee_info(location);

        self.fn_call_analysis(callee_info);
    }

    fn fn_call_analysis(&mut self, callee_info: CalleeInfo<'tcx>) {
        // Account for function call overhead
        self.state.add_step();

        if try_dispatch_to_specifications(self, &callee_info) {
            // we found specs for the call
        } else if self.tcx.is_mir_available(&callee_info.callee_def_id) {
            self.analyze_with_available_mir(&callee_info);
        } else {
            println!(
                "Cannot analyze: {}.\nComplementary info:\n\tsubsts_ref: {:?}\n\targs_type:\n",
                self.tcx.def_path_str(callee_info.callee_def_id),
                callee_info.substs_ref,
            );
            for arg in callee_info.args.iter() {
                println!(
                    "\t\t{:#?}",
                    self.state.get_type_info_for_place(&arg.place().unwrap())
                );
            }

            *self.analysis_state.borrow_mut() = AnalysisState::Failure;
        }
    }

    fn analyze_with_available_mir(&mut self, callee_info: &CalleeInfo<'tcx>) {
        let target_mir = self.tcx.optimized_mir(callee_info.callee_def_id);

        // Detect loops in analyzed function
        if target_mir.is_cfg_cyclic() {
            println!(
                "Loop detected in function {}, loops are not supported",
                self.tcx.def_path_str(callee_info.callee_def_id)
            );
            *self.analysis_state.borrow_mut() = AnalysisState::Failure;
            return;
        }

        self.analyze_with_mir(callee_info, target_mir);
    }

    fn analyze_with_mir(&mut self, callee_info: &CalleeInfo<'tcx>, target_mir: &Body<'tcx>) {
        // Fill the calling context (arguments) for the function to be analyzes
        let mut caller_context_args_type_info: Vec<TypeInfo> = Vec::new();

        for arg in callee_info.args.iter() {
            match arg {
                Operand::Copy(place) | Operand::Move(place) => {
                    caller_context_args_type_info
                        .push(self.state.get_type_info_for_place(place).unwrap());
                }
                Operand::Constant(constant) => {
                    if let Some((def_id, substs_ref)) = arg.const_fn_def() {
                        // Constant function,
                        // cf. https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/enum.Operand.html#method.const_fn_def
                        caller_context_args_type_info.push(TypeInfo::new(
                            self.tcx.mk_fn_def(def_id, substs_ref),
                            self.tcx,
                        ));
                    } else {
                        caller_context_args_type_info.push(TypeInfo::new(constant.ty(), self.tcx));
                    }
                }
            }
        }

        // Analyze the target function
        let mut results = CostAnalysis::new_with_init(
            self.tcx,
            self.pallet,
            self.events_variants,
            callee_info.callee_def_id,
            caller_context_args_type_info,
            self.analysis_state.clone(),
        )
        .into_engine(self.tcx, target_mir)
        .pass_name("cost_analysis")
        .iterate_to_fixpoint()
        .into_results_cursor(target_mir);

        let updated_state;
        {
            let self_success_state = self.analysis_state.borrow();
            // Retrieve target function analysis success flag and
            updated_state = self_success_state.and(&results.analysis().analysis_state.borrow());
        }
        *self.analysis_state.borrow_mut() = updated_state;

        // Retrieve last state of callee function as its summary
        let end_state = if let Some((last, _)) = reverse_postorder(target_mir).last() {
            results.seek_to_block_end(last);
            Some(results.get().clone())
        } else {
            unreachable!();
        };

        if let Some(end_state) = end_state {
            // Update caller function state
            self.state.inter_join(&end_state);
        }
    }

    pub(crate) fn analyze_closure_call(&mut self, callee_info: &CalleeInfo<'tcx>) {
        // We apply the action of {"std::ops::FnOnce", "std::ops::Fn", "std::ops::FnMut"}.call/call_once

        // First arg is Closure of FnDef, second is a packed tuple of their arguments
        assert!(callee_info.args.len() == 2);

        // Extract closure adt
        let closure_adt_place = callee_info.args[0].place().unwrap();
        let closure_adt = self
            .state
            .get_type_info_for_place(&closure_adt_place)
            .unwrap();
        let (closure_fn_ptr, closure_substs_ref) =
            if let TyKind::Closure(def_id, substs_ref) = closure_adt.get_ty().kind() {
                (*def_id, substs_ref)
            } else if let TyKind::FnDef(def_id, substs_ref) = closure_adt.get_ty().kind() {
                (*def_id, substs_ref)
            } else {
                unreachable!();
            };

        // The arguments are packed in a Tuple, we need to
        // create the projections on the local to target the places
        // to unpack them
        let closure_args_place = callee_info.args[0].place().unwrap();
        let args_count = self
            .state
            .get_type_info_for_place(&closure_args_place)
            .unwrap()
            .get_members()
            .len();
        let mut closure_args_operands = Vec::new();
        assert!(closure_args_place.projection.is_empty());
        // We can forge direct field access
        for i in 0..args_count {
            let field_ty = self
                .state
                .get_type_info_for_place(&closure_args_place)
                .unwrap()
                .get_member(i)
                .unwrap()
                .get_ty();
            let arg_field_projection_place_elem =
                vec![ProjectionElem::Field(mir::Field::from_usize(i), field_ty)];
            let closure_arg_place =
                closure_args_place.project_deeper(&arg_field_projection_place_elem, self.tcx);
            let closure_arg_operand = Operand::Move(closure_arg_place);

            closure_args_operands.push(closure_arg_operand);
        }

        // Put closure adt operand in front of argument list
        closure_args_operands.insert(0, callee_info.args[0].clone());

        // Used for debug, let's set the closure's call location where
        // is called {"std::ops::FnOnce", "std::ops::Fn", "std::ops::FnMut"}.call/call_once
        let closure_call_location = callee_info.location;

        // The return value of the closure will eventually get there as well
        let closure_call_destination = callee_info.destination;

        let closure_callee_info = CalleeInfo {
            location: closure_call_location,
            args: closure_args_operands,
            destination: closure_call_destination,
            callee_def_id: closure_fn_ptr,
            substs_ref: closure_substs_ref,
        };

        self.fn_call_analysis(closure_callee_info.clone());
    }

    fn get_callee_info(&self, location: Location) -> CalleeInfo<'tcx> {
        let body = self.tcx.optimized_mir(self.def_id);
        let terminator = body.stmt_at(location).right().unwrap();
        match &terminator.kind {
            TerminatorKind::Call {
                func,
                args,
                destination,
                ..
            } if let Operand::Constant(c) = func && let TyKind::FnDef(callee_def_id, substs_ref) = *c.ty().kind()
             => CalleeInfo {
                 location: Some(location),
                 args: (*args).clone(),
                 destination: Some(*destination),
                 callee_def_id,
                 substs_ref
             },
            _ => unreachable!("{:?}", terminator.kind),
        }
    }

    fn overwrite_place_to(&mut self, place_from: &Place, place_to: &Place) {
        let place_from_type_info = self.state.get_type_info_for_place(place_from);

        if place_from_type_info == self.state.get_type_info_for_place(place_to) {
            return;
        }

        if let Some(place_from_type_info) = place_from_type_info {
            if place_to.projection.is_empty() {
                // Reflect the whole type
                self.state.locals_info[place_to.local].set_local_info(place_from_type_info);
            } else {
                // Reflect the type of the given field of "place_from" to the given field of "place_to"
                if let ProjectionElem::Field(field ,_) = place_to.projection.last().unwrap()
                && self.state.locals_info[place_to.local].type_info.has_members()
                {
                    self.state.locals_info[place_to.local].set_member(field.index(), place_from_type_info);
                } else if let ProjectionElem::Deref = place_to.projection.last().unwrap() {
                    // Reflect the whole reference
                    self.state.locals_info[place_to.local].set_local_info(place_from_type_info);
                }
            }
        }
    }
}

impl<'tcx> Visitor<'tcx> for TransferFunction<'tcx, '_, '_> {
    fn visit_rvalue(&mut self, rvalue: &Rvalue<'tcx>, location: Location) {
        match rvalue {
            Rvalue::BinaryOp(_, box (lhs, rhs)) | Rvalue::CheckedBinaryOp(_, box (lhs, rhs)) => {
                self.visit_operand(lhs, location);
                self.visit_operand(rhs, location);

                self.state.add_step();
            }
            Rvalue::UnaryOp(_, op) => {
                self.visit_operand(op, location);

                self.state.add_step();
            }
            _ => self.super_rvalue(rvalue, location),
        }
    }

    fn visit_statement(&mut self, statement: &Statement<'tcx>, location: Location) {
        let Statement { kind, .. } = statement;

        match kind {
            StatementKind::Assign(box (place_to, r_value)) if let Rvalue::Use(operand) = r_value => {
                match operand {
                    Operand::Copy(place_from)
                    | Operand::Move(place_from) => {
                        self.overwrite_place_to(place_from, place_to);
                    }
                    Operand::Constant(_) => {
                        if let Some((def_id, substs_ref)) = operand.const_fn_def() {
                            // In case of constant function, update with the function type
                            self.state.locals_info[place_to.local].set_ty(self.tcx.mk_fn_def(def_id, substs_ref));
                        } else if let Some(constant) = operand.constant() {
                            // In case of standard constant, update with its type
                            self.state.locals_info[place_to.local].set_ty(constant.ty());
                        }
                    }
                }
            }
            StatementKind::Assign(box (place_to, r_value)) if let Rvalue::Ref(_, _, place_from) = r_value => {
                // Replace references by their underlying types
                self.overwrite_place_to(place_from, place_to);
            }
            _ => self.super_statement(statement, location),
        }
    }

    fn visit_terminator(&mut self, terminator: &Terminator<'tcx>, location: Location) {
        let Terminator { kind, .. } = terminator;

        match kind {
            TerminatorKind::Call {
                func: Operand::Constant(c),
                ..
            } if let TyKind::FnDef(target_def_id, substs) = c.ty().kind() => {

                self.visit_fn_call(location);
            }
            _ => self.super_terminator(terminator, location),
        }
    }
}
