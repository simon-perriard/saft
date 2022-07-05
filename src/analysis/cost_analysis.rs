use super::cost_domain::{ExtendedCostAnalysisDomain, LocalInfo, LocalsInfo, TypeInfo};
use super::cost_language::{Cost, CostParameter};
use super::events_variants_domain::Variants;
use super::pallet::Pallet;
//use super::specifications::needs_early_catch;
//use super::specifications::storage_actions_specs::HasAccessCost;
use crate::analysis::events_variants_domain::EventVariantsDomain;
//use crate::analysis::specifications::dispatch_to_specifications;
use rustc_middle::mir::{
    self, traversal::*, visit::*, BasicBlock, Body, Local, Location, Operand, Place,
    ProjectionElem, Rvalue, Statement, StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::{Instance, ParamEnv, subst::SubstsRef, Ty, TyCtxt, TyKind};
use rustc_mir_dataflow::{Analysis, AnalysisDomain, Backward, CallReturnPlaces, Forward};
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
    pallet: &'inter Pallet,
    events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
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
    fn t_visit_fn_call(&mut self, location: Location) {
        let callee_info = self.get_callee_info(location);

        if self.is_deposit_event(callee_info.callee_def_id) {
            // Filtering event deposit, pallet::deposit_event will later call frame_system::deposit_event,
            // but we can only catch the variant of the Event enum now. Or we need to pass a calling context for
            // further analysis.
            //self.analyze_deposit_event(callee_info);
            println!("EVENT");
        }
        /* else if needs_early_catch(&self.tcx.def_path_str(callee_info.callee_def_id)) {
            self.analyze_with_specifications(callee_info);
        }*/
        else {
            self.t_fn_call_analysis(callee_info);
        }
    }

    fn t_fn_call_analysis(&mut self, callee_info: CalleeInfo<'tcx>) {
        // Account for function call overhead
        self.state.add_steps(Cost::Scalar(1));

        if self.tcx.is_mir_available(callee_info.callee_def_id) {
            self.analyze_with_available_mir(&callee_info);
        } else if self.is_closure_call(callee_info.callee_def_id) {
            self.analyze_closure_call(&callee_info);
        } else {
            println!("SPECS");
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

    fn analyze_closure_call(&mut self, callee_info: &CalleeInfo<'tcx>) {
        // We apply the action of {"std::ops::FnOnce", "std::ops::Fn", "std::ops::FnMut"}.call

        println!("CLOSURE");
    }

    fn is_deposit_event(&self, target_def_id: DefId) -> bool {
        let path = self.tcx.def_path_str(target_def_id);
        path.starts_with("pallet::Pallet") && path.ends_with("deposit_event")
    }

    fn is_closure_call(&self, target_def_id: DefId) -> bool {
        let path: &str = &self.tcx.def_path_str(target_def_id);
        let closure_calls_list = vec!["std::ops::FnOnce", "std::ops::Fn", "std::ops::FnMut"];

        for closure_call in closure_calls_list {
            if path.contains(closure_call) {
                return true;
            }
        }

        false
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

    fn get_callee_args_types(&self, callee_info: &CalleeInfo<'tcx>) -> Vec<LocalInfo<'tcx>> {
        callee_info
            .args
            .iter()
            .map(|arg| match arg.place() {
                Some(place) => {
                    LocalInfo::from_type_info(self.state.get_type_info_for_place(&place).unwrap())
                }
                None => match arg.const_fn_def() {
                    Some((def_id, _)) => LocalInfo::new(self.tcx.type_of(def_id), self.tcx),
                    None => LocalInfo::new(arg.constant().unwrap().ty(), self.tcx),
                },
            })
            .collect::<Vec<_>>()
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

                self.state.add_steps(Cost::Scalar(1));
            }
            Rvalue::UnaryOp(_, op) => {
                self.visit_operand(op, location);

                self.state.add_steps(Cost::Scalar(1));
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

                self.t_visit_fn_call(location);
            }
            _ => self.super_terminator(terminator, location),
        }
    }
}