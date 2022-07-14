use super::cost_domain::{ExtendedCostAnalysisDomain, FreshIdProvider, LocalInfo};
use super::pallet::Pallet;
use super::specifications_v2::try_dispatch_to_specifications;
use crate::analysis::events_variants_domain::EventVariantsDomain;
use rustc_middle::mir::{
    traversal::*, visit::*, BasicBlock, Body, Location, Operand, Place, Rvalue, Statement,
    StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::{subst::SubstsRef, TyCtxt, TyKind};
use rustc_mir_dataflow::{Analysis, AnalysisDomain, CallReturnPlaces, Forward};
use rustc_span::def_id::DefId;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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
    pub args_type_info: Vec<LocalInfo<'tcx>>,
    pub caller_args_operands: Option<Vec<Operand<'tcx>>>,
    pub destination: Option<Place<'tcx>>,
    pub callee_def_id: DefId,
    pub substs_ref: SubstsRef<'tcx>,
}

pub(crate) struct CostAnalysis<'tcx, 'inter> {
    tcx: TyCtxt<'tcx>,
    pallet: &'inter Pallet,
    events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
    def_id: DefId,
    caller_context_args_type_info: Vec<LocalInfo<'tcx>>,
    pub analysis_success_state: Rc<RefCell<AnalysisState>>,
    fresh_var_id: FreshIdProvider,
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
            Rc::new(RefCell::new(0)),
        )
    }

    fn new_with_init(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
        def_id: DefId,
        caller_context_args_type_info: Vec<LocalInfo<'tcx>>,
        state: Rc<RefCell<AnalysisState>>,
        fresh_var_id: FreshIdProvider,
    ) -> Self {
        CostAnalysis {
            tcx,
            pallet,
            events_variants,
            def_id,
            caller_context_args_type_info,
            analysis_success_state: state,
            fresh_var_id,
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
            self.analysis_success_state.clone(),
            self.fresh_var_id.clone(),
        )
    }
}

pub(crate) struct TransferFunction<'tcx, 'inter, 'transformer> {
    pub tcx: TyCtxt<'tcx>,
    pub pallet: &'inter Pallet,
    pub events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
    pub def_id: DefId,
    pub state: &'transformer mut ExtendedCostAnalysisDomain<'tcx>,
    pub analysis_success_state: Rc<RefCell<AnalysisState>>,
    pub fresh_var_id: FreshIdProvider,
}

impl<'tcx, 'inter, 'transformer> TransferFunction<'tcx, 'inter, 'transformer> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
        def_id: DefId,
        state: &'transformer mut ExtendedCostAnalysisDomain<'tcx>,
        analysis_success_state: Rc<RefCell<AnalysisState>>,
        fresh_var_id: FreshIdProvider,
    ) -> Self {
        TransferFunction {
            tcx,
            pallet,
            events_variants,
            def_id,
            state,
            analysis_success_state,
            fresh_var_id,
        }
    }
}

impl<'tcx, 'inter> AnalysisDomain<'tcx> for CostAnalysis<'tcx, 'inter> {
    type Domain = ExtendedCostAnalysisDomain<'tcx>;
    const NAME: &'static str = "CostAnalysis";

    type Direction = Forward;

    fn bottom_value(&self, body: &Body<'tcx>) -> Self::Domain {
        let mut state = ExtendedCostAnalysisDomain::new(self.tcx, body, self.fresh_var_id.clone());
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
        if *self.analysis_success_state.borrow() == AnalysisState::Success {
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
        if *self.analysis_success_state.borrow() == AnalysisState::Success {
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

        self.fn_call_analysis(callee_info, false);
    }

    pub(crate) fn fn_call_analysis(
        &mut self,
        callee_info: CalleeInfo<'tcx>,
        run_in_isolation: bool,
    ) -> ExtendedCostAnalysisDomain<'tcx> {
        // Account for function call overhead
        self.state.add_step();

        if let Some(end_state) = try_dispatch_to_specifications(self, &callee_info) {
            // we found specs for the call
            end_state
        } else if self.tcx.is_mir_available(&callee_info.callee_def_id) {
            self.analyze_with_available_mir(&callee_info, run_in_isolation)
        } else {
            //rustc_middle::mir::pretty::write_mir_fn(self.tcx, &self.tcx.optimized_mir(self.def_id), &mut |_, _| Ok(()), &mut std::io::stdout());
            println!(
                "Cannot analyze: {}.\nComplementary info:\n\tsubsts_ref: {:#?}\n\targs_type:\n",
                self.tcx.def_path_str(callee_info.callee_def_id),
                callee_info.substs_ref,
            );
            for arg in callee_info.args_type_info.iter() {
                println!("\t\t{:#?}", arg);
            }
            *self.analysis_success_state.borrow_mut() = AnalysisState::Failure;
            (*self.state).clone()
        }
    }

    fn analyze_with_available_mir(
        &mut self,
        callee_info: &CalleeInfo<'tcx>,
        run_in_isolation: bool,
    ) -> ExtendedCostAnalysisDomain<'tcx> {
        let target_mir = self.tcx.optimized_mir(callee_info.callee_def_id);

        // Detect loops in analyzed function
        if target_mir.is_cfg_cyclic() {
            println!(
                "Loop detected in function {}, loops are not supported",
                self.tcx.def_path_str(callee_info.callee_def_id)
            );
            *self.analysis_success_state.borrow_mut() = AnalysisState::Failure;
            return (*self.state).clone();
        }

        self.analyze_with_mir(callee_info, target_mir, run_in_isolation)
    }

    fn analyze_with_mir(
        &mut self,
        callee_info: &CalleeInfo<'tcx>,
        target_mir: &Body<'tcx>,
        run_in_isolation: bool,
    ) -> ExtendedCostAnalysisDomain<'tcx> {
        // Analyze the target function
        let mut results = CostAnalysis::new_with_init(
            self.tcx,
            self.pallet,
            self.events_variants,
            callee_info.callee_def_id,
            callee_info.args_type_info.clone(),
            self.analysis_success_state.clone(),
            self.fresh_var_id.clone(),
        )
        .into_engine(self.tcx, target_mir)
        .pass_name("cost_analysis")
        .iterate_to_fixpoint()
        .into_results_cursor(target_mir);

        let updated_state;
        {
            let self_success_state = self.analysis_success_state.borrow();
            // Retrieve target function analysis success flag and
            updated_state =
                self_success_state.and(&results.analysis().analysis_success_state.borrow());
        }
        *self.analysis_success_state.borrow_mut() = updated_state;

        // Retrieve last state of callee function as its summary
        let end_state = if let Some((last, _)) = reverse_postorder(target_mir).last() {
            results.seek_to_block_end(last);
            Some(results.get().clone())
        } else {
            unreachable!();
        };

        if !run_in_isolation && let Some(end_state) = end_state.clone() {
            // Update caller function state
            self.state.inter_join(&end_state);

            // Pass the return LocalInfo to the caller
            if let Some(dest_place) = callee_info.destination {
                assert!(dest_place.projection.is_empty());

                let callee_return_place = Place::return_place();
                let returned_local_info = end_state.get_local_info_for_place(&callee_return_place).unwrap();
                self.state.locals_info[dest_place.local].set_length_of(returned_local_info);
                self.state.locals_info[dest_place.local].fill_with_inner_size(self.tcx);
            }
        }

        end_state.unwrap()
    }

    pub(crate) fn analyze_closure_call(&mut self, callee_info: &CalleeInfo<'tcx>) {
        // We apply the action of {"std::ops::FnOnce", "std::ops::Fn", "std::ops::FnMut"}.call/call_once/call_mut

        // First arg is Closure of FnDef, second is a packed tuple of their arguments
        assert!(callee_info.args_type_info.len() == 2);

        // Extract closure adt
        let closure_adt = callee_info.args_type_info[0].clone();
        let (closure_fn_ptr, closure_substs_ref) =
            if let TyKind::Closure(def_id, substs_ref) = closure_adt.get_ty().kind() {
                (*def_id, substs_ref)
            } else if let TyKind::FnDef(def_id, substs_ref) = closure_adt.get_ty().kind() {
                (*def_id, substs_ref)
            } else {
                unreachable!(
                    "Caller: {} --- {:#?}",
                    self.tcx.def_path_str(self.def_id),
                    callee_info
                );
            };

        // The arguments are packed in a Tuple, we need to
        // create the projections on the local to target the places
        // to unpack them
        let closure_args_packed = callee_info.args_type_info[1].clone();
        let args_count = closure_args_packed.get_members().len();
        let mut closure_args_type_info = Vec::new();

        // We can simply unpack the Tuple,
        // MIR does direct field access as projection for this
        for i in 0..args_count {
            closure_args_type_info.push(closure_args_packed.get_member(i).unwrap().clone());
        }

        // Put closure adt operand in front of argument list
        closure_args_type_info.insert(0, callee_info.args_type_info[0].clone());

        // Used for debug, let's set the closure's call location where
        // is called {"std::ops::FnOnce", "std::ops::Fn", "std::ops::FnMut"}.call/call_once
        let closure_call_location = callee_info.location;

        // The return value of the closure will eventually get there as well
        let closure_call_destination = callee_info.destination;

        let closure_callee_info = CalleeInfo {
            location: closure_call_location,
            args_type_info: closure_args_type_info,
            caller_args_operands: None,
            destination: closure_call_destination,
            callee_def_id: closure_fn_ptr,
            substs_ref: closure_substs_ref,
        };

        self.fn_call_analysis(closure_callee_info.clone(), false);
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
             => {
                let mut args_type_info: Vec<LocalInfo> = Vec::new();
                let mut caller_args_operands: Vec<Operand> = Vec::new();

                for arg in args.iter() {
                    caller_args_operands.push((*arg).clone());
                    match arg {
                        Operand::Copy(place) | Operand::Move(place) => {

                            //Panic if there is a mutable reference
                            /*let local = body.local_decls[place.local].clone();
                            if let TyKind::Ref(_, _, mutability) = local.ty.kind() && *mutability == rustc_middle::mir::Mutability::Mut {
                                rustc_middle::mir::pretty::write_mir_fn(self.tcx, &body, &mut |_, _| Ok(()), &mut std::io::stdout());
                                panic!("{:?}", (location, body.local_decls[place.local].ty.kind()));
                            }*/

                            args_type_info
                                .push(self.state.get_local_info_for_place(place).unwrap());
                        }
                        Operand::Constant(constant) => {
                            if let Some((def_id, substs_ref)) = arg.const_fn_def() {
                                // Constant function,
                                // cf. https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/enum.Operand.html#method.const_fn_def
                                args_type_info.push(LocalInfo::new(
                                    self.tcx.mk_fn_def(def_id, substs_ref),
                                    self.tcx,
                                    None,
                                    self.fresh_var_id.clone()
                                ));
                            } else {
                                args_type_info.push(LocalInfo::new(constant.ty(), self.tcx, None, self.fresh_var_id.clone()));
                            }
                        }
                    }
                }
                CalleeInfo {
                 location: Some(location),
                 args_type_info,
                 caller_args_operands: Some(caller_args_operands),
                 destination: Some(*destination),
                 callee_def_id,
                 substs_ref
             }
            },
            _ => unreachable!("{:?}", terminator.kind),
        }
    }

    fn overwrite_place_to(&mut self, place_from: &Place, place_to: &Place) {
        let place_from_type_info = self.state.get_local_info_for_place(place_from);

        if place_from_type_info == self.state.get_local_info_for_place(place_to) {
            if let Some(place_from_type_info) = place_from_type_info {
                self.state
                    .forward_symbolic_attributes(place_to, place_from_type_info);

                if place_to.projection.is_empty() {
                    self.state.locals_info[place_to.local].fill_with_inner_size(self.tcx);
                }
            }
            return;
        }

        if let Some(place_from_type_info) = place_from_type_info {
            self.state
                .set_local_info_for_place(place_to, place_from_type_info);
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
                    Operand::Constant(box const_operand) => {
                        if let Some((def_id, substs_ref)) = operand.const_fn_def() {
                            // In case of constant function, update with the function type
                            self.state.set_local_info_for_place(place_to, LocalInfo::new(self.tcx.mk_fn_def(def_id, substs_ref), self.tcx, Some(const_operand.span), self.fresh_var_id.clone()));
                        } else if let Some(constant) = operand.constant() {
                            // In case of standard constant, update with its type
                            self.state.set_local_info_for_place(place_to, LocalInfo::new(constant.ty(), self.tcx, Some(const_operand.span), self.fresh_var_id.clone()));
                        } else {
                            panic!();
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
