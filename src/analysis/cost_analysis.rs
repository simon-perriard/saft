use super::cost_domain::{CostDomain, ExtendedCostAnalysisDomain, LocalInfo, LocalsInfo};
use super::cost_language::{Cost, CostParameter};
use super::events_variants_domain::Variants;
use super::pallet::Pallet;
//use super::specifications::needs_early_catch;
//use super::specifications::storage_actions_specs::HasAccessCost;
use crate::analysis::events_variants_domain::EventVariantsDomain;
//use crate::analysis::specifications::dispatch_to_specifications;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{
    self, traversal::*, visit::*, BasicBlock, Body, Local, Location, Operand, Place,
    ProjectionElem, Rvalue, Statement, StatementKind, Terminator, TerminatorKind,
    VarDebugInfoContents,
};
use rustc_middle::ty::{subst::SubstsRef, Ty, TyCtxt, TyKind};
use rustc_mir_dataflow::{Analysis, AnalysisDomain, CallReturnPlaces, Forward};
use rustc_span::def_id::DefId;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::vec;

//TODO REPORT: we use a caching mechanism to speed up a subset of the calls
// we cache a subset of the calls of the analysis, those considered as slow
// fast ones are specs
pub(crate) type SummaryKey<'tcx> = (DefId, Vec<LocalInfo<'tcx>>);
pub(crate) type Summary<'tcx> = HashMap<SummaryKey<'tcx>, Option<CostDomain>>;

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
    pub summaries: Rc<RefCell<Summary<'tcx>>>,
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
            Rc::new(RefCell::new(HashMap::new())),
            Rc::new(RefCell::new(AnalysisState::Success)),
        )
    }

    fn new_with_init(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
        def_id: DefId,
        summaries: Rc<RefCell<Summary<'tcx>>>,
        state: Rc<RefCell<AnalysisState>>,
    ) -> Self {
        CostAnalysis {
            tcx,
            pallet,
            events_variants,
            def_id,
            summaries,
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
            self.summaries.clone(),
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
    pub summaries: Rc<RefCell<Summary<'tcx>>>,
    pub def_id: DefId,
    pub domain_state: &'transformer mut ExtendedCostAnalysisDomain<'tcx>,
    pub analysis_state: Rc<RefCell<AnalysisState>>,
}

impl<'tcx, 'inter, 'transformer> TransferFunction<'tcx, 'inter, 'transformer> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
        summaries: Rc<RefCell<Summary<'tcx>>>,
        def_id: DefId,
        domain_state: &'transformer mut ExtendedCostAnalysisDomain<'tcx>,
        analysis_state: Rc<RefCell<AnalysisState>>,
    ) -> Self {
        TransferFunction {
            tcx,
            pallet,
            events_variants,
            summaries,
            def_id,
            domain_state,
            analysis_state,
        }
    }

    pub fn get_local_info_for_place(&self, place: &Place) -> Option<LocalInfo<'tcx>> {
        self.domain_state.get_local_info_for_place(place)
    }

    pub fn get_summary_for_key(&self, key: &SummaryKey<'tcx>) -> CostDomain {
        (*self.summaries.borrow().get(key).unwrap())
            .clone()
            .unwrap()
    }
}

impl<'tcx, 'inter> AnalysisDomain<'tcx> for CostAnalysis<'tcx, 'inter> {
    type Domain = ExtendedCostAnalysisDomain<'tcx>;
    const NAME: &'static str = "CostAnalysis";

    type Direction = Forward;

    fn bottom_value(&self, body: &Body<'tcx>) -> Self::Domain {
        ExtendedCostAnalysisDomain::new(self.tcx, body)
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

        if self.is_storage_field_access(callee_info.callee_def_id) {
            // Filtering storage access, we need to catch it now otherwise we lose information about which
            // field is accessed.
            self.analyze_storage_access(callee_info);
        } else if self.is_deposit_event(callee_info.callee_def_id) {
            // Filtering event deposit, pallet::deposit_event will later call frame_system::deposit_event,
            // but we can only catch the variant of the Event enum now. Or we need to pass a calling context for
            // further analysis.
            self.analyze_deposit_event(callee_info);
        }
        /* else if needs_early_catch(&self.tcx.def_path_str(callee_info.callee_def_id)) {
            self.analyze_with_specifications(callee_info);
        }*/
        else {
            self.t_fn_call_analysis(callee_info, true);
        }
    }

    fn t_fn_call_analysis(&mut self, callee_info: CalleeInfo<'tcx>, account_for_cost_now: bool) {
        // Account for function call overhead
        self.domain_state.add_steps(Cost::Scalar(1));

        // TODO: refactor summary mechanism
        /*let summary_key = self.get_summary_key_for_callee_info(&callee_info);

        if self.summaries.borrow().contains_key(&summary_key) {
            // We already have the summary for this function, retrieve it and return
            let summaries = self.summaries.borrow();
            let summary = summaries.get(&summary_key).unwrap();

            if let Some(summary) = summary {
                // Add the cost of calling the target function
                if account_for_cost_now {
                    self.domain_state.inter_join(summary);
                }
            } else {
                // we are in a recursive call, just ignore it
                println!("Recursive calls not supported.");
                *self.analysis_state.borrow_mut() = AnalysisState::Failure;
            }
        } else*/
        if self.tcx.is_mir_available(callee_info.callee_def_id) {
            // We don't have the summary but MIR is available, we need to analyze the function
            self.analyze_with_available_mir(&callee_info, account_for_cost_now);
        } else if self.is_closure_call(callee_info.callee_def_id) {
            self.analyze_closure_call(callee_info);
        } else {
            self.analyze_with_specifications(callee_info);
        }
    }

    fn analyze_closure_as_argument(
        &mut self,
        closure_def_id: DefId,
        closure_substs_ref: SubstsRef<'tcx>,
        args: Option<Vec<Operand<'tcx>>>,
    ) -> SummaryKey<'tcx> {
        let closure_info = CalleeInfo {
            // We do not know at what point the closure is called
            location: None,

            // precise enough args type will be inferred by the closure body
            args: match args {
                Some(args) => args,
                None => vec![],
            },
            // cannot keep the storage access return type as it is not the same as closure's
            // precise enough args type will be inferred by the closure body
            destination: None,

            callee_def_id: closure_def_id,

            substs_ref: closure_substs_ref,
        };
        self.t_fn_call_analysis(closure_info.clone(), false);
        self.get_summary_key_for_callee_info(&closure_info)
    }

    fn analyze_storage_access(&mut self, callee_info: CalleeInfo<'tcx>) {
        // Account for function call overhead
        self.domain_state.add_steps(Cost::Scalar(1));

        // From Subtrate storage access implementation, if there is a closure it is as last argument
        let maybe_closure_arg = callee_info.args.last().and_then(|arg| {
            arg.place().map(|place| {
                let local_type = self.get_local_info_for_place(&place).unwrap();
                (Some(vec![(*arg).clone()]), local_type.get_ty().kind())
            })
        });

        // First analyze the closure that may be in the param but do not account for its cost yet
        // we do this in the specifications

        let maybe_closure_def_id =
            if let Some((args, TyKind::Closure(closure_def_id, closure_substs_ref))) =
                maybe_closure_arg
            {
                // Storage access functions may have closures as parameters, we need to analyze them
                Some(self.analyze_closure_as_argument(*closure_def_id, closure_substs_ref, args))
            } else {
                None
            };

        self.dispatch_to_storage_access_specifications(&callee_info, maybe_closure_def_id);
    }

    fn analyze_deposit_event(&mut self, callee_info: CalleeInfo<'tcx>) {
        // Account for function call overhead
        self.domain_state.add_steps(Cost::Scalar(1));

        let args = callee_info.args;
        let location = callee_info.location;

        let body = self.tcx.optimized_mir(self.def_id);

        if let TyKind::Adt(adt_def, _) = args[0].ty(body, self.tcx).kind() {
            let event_variants = self
                .events_variants
                .get(&self.def_id)
                .unwrap()
                .get(location.unwrap())
                .unwrap();

            match event_variants {
                Variants::Variant(variant_id) => {
                    let ty = args[0].ty(body, self.tcx);

                    let cost = match self
                        .tcx
                        .layout_of(self.tcx.param_env(adt_def.did()).and(ty))
                    {
                        Ok(ty_and_layout) => {
                            let layout = ty_and_layout.layout;
                            match layout.variants() {
                                rustc_target::abi::Variants::Single { .. } => {
                                    Cost::Scalar(ty_and_layout.layout.size().bytes())
                                }
                                rustc_target::abi::Variants::Multiple { variants, .. } => {
                                    let variant_layout = variants[*variant_id];
                                    Cost::Scalar(variant_layout.size().bytes())
                                }
                            }
                        }
                        Err(_) => {
                            let variant = adt_def.variant(*variant_id);
                            Cost::Parameter(CostParameter::SizeOf(
                                self.tcx.def_path_str(variant.def_id),
                            ))
                        }
                    };

                    self.domain_state.add_events(cost);
                }
                Variants::Or(_, _) => {
                    let cost = event_variants
                        .flatten_or()
                        .iter()
                        .map(|variant_id| {
                            let variant = adt_def.variant(*variant_id);
                            // For now add the variant size as CostParameter
                            Cost::Parameter(CostParameter::SizeOf(
                                self.tcx.def_path_str(variant.def_id),
                            ))
                        })
                        .reduce(|accum, item| accum.max(item))
                        .unwrap();
                    self.domain_state.add_events(cost);
                }
            }
        }
    }

    fn analyze_with_available_mir(
        &mut self,
        callee_info: &CalleeInfo<'tcx>,
        account_for_cost_now: bool,
    ) {
        // Initialize the summary to None so we can detect a recursive call later
        /*let summary_key = self.get_summary_key_for_callee_info(callee_info);
        self.summaries.borrow_mut().insert(summary_key, None);*/

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

        self.analyze_with_mir(callee_info, target_mir, account_for_cost_now);
    }

    fn analyze_with_mir(
        &mut self,
        callee_info: &CalleeInfo<'tcx>,
        target_mir: &Body<'tcx>,
        account_for_cost_now: bool,
    ) {
        return;
        // Analyze the target function
        let mut results = CostAnalysis::new_with_init(
            self.tcx,
            self.pallet,
            self.events_variants,
            callee_info.callee_def_id,
            self.summaries.clone(),
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
            if account_for_cost_now {
                // Update caller function state
                self.domain_state.inter_join(&end_state);
            }

            // Add the callee function summary to our summaries map
            /*let summary_key = self.get_summary_key_for_callee_info(callee_info);
            self.summaries
                .borrow_mut()
                .insert(summary_key, Some(end_state.costs));*/
        }
    }

    fn analyze_with_specifications(&mut self, callee_info: CalleeInfo<'tcx>) {
        // Account for function call overhead
        self.domain_state.add_steps(Cost::Scalar(1));
        // No MIR available, but CostParameterally account for the call cost

        if *self.analysis_state.borrow() == AnalysisState::Success {
            println!("SPECS");
            //dispatch_to_specifications(self, callee_info);
        }
    }

    fn analyze_closure_call(&mut self, callee_info: CalleeInfo<'tcx>) {
        // First arg is closure or action
        let callee_def_id = match self
            .domain_state
            .locals_info
            .get(callee_info.args[0].place().unwrap().local)
            .unwrap()
            .get_ty()
            .kind()
        {
            TyKind::Closure(closure_def_id, _) => Some(*closure_def_id),
            TyKind::FnDef(def_id, _) => Some(*def_id),
            TyKind::Ref(_, _, _) => None,  //No further analysis needed
            TyKind::Projection(_) => None, //No further analysis needed
            _ => unreachable!(),
        };

        if let Some(callee_def_id) = callee_def_id {
            let mut closure_args = callee_info.args.clone();
            // First arg is closure, we remove it to keep only its args
            closure_args.pop();
            let closure_info = CalleeInfo {
                callee_def_id,
                args: closure_args,
                ..callee_info
            };
            self.t_fn_call_analysis(closure_info, true);
        }
    }

    fn dispatch_to_storage_access_specifications(
        &mut self,
        callee_info: &CalleeInfo<'tcx>,
        closure_summary_key: Option<SummaryKey<'tcx>>,
    ) {
        let def_id = callee_info.callee_def_id;
        let substs = callee_info.substs_ref;

        if !self.is_storage_field_access(def_id) {
            panic!(
                "This is not a storage call: {}",
                self.tcx.def_path_str(def_id)
            );
        }

        let pallet = self.pallet;
        let tcx = self.tcx;
        let key = tcx.def_key(def_id);
        let parent_def_id = DefId {
            index: key.parent.unwrap(),
            ..def_id
        };
        let generics = tcx.generics_of(def_id);
        let parent_substs = &substs[..generics.parent_count.min(substs.len())];

        if let TyKind::Adt(adt_def_data, _) = tcx.type_of(parent_def_id).kind() {
            let reconstructed_ty = tcx.mk_adt(*adt_def_data, tcx.intern_substs(parent_substs));

            let ty_field_hash_map = pallet
                .fields
                .iter()
                .map(|(field_def_id, field)| (tcx.type_of(field_def_id), (*field).clone()))
                .fold(HashMap::new(), |mut accum, (field_ty, field)| {
                    accum.insert(field_ty, field);
                    accum
                });

            let field = ty_field_hash_map.get(&reconstructed_ty).unwrap();

            //field.get_access_cost(self, callee_info, closure_summary_key)
        } else {
            unreachable!();
        }
    }

    fn is_storage_field_access(&self, def_id: DefId) -> bool {
        self.tcx
            .def_path_str(def_id)
            .starts_with("frame_support::pallet_prelude::Storage")
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
                Some(place) => self.get_local_info_for_place(&place).unwrap(),
                None => match arg.const_fn_def() {
                    Some((def_id, _)) => LocalInfo::new(self.tcx.type_of(def_id), self.tcx),
                    None => LocalInfo::new(arg.constant().unwrap().ty(), self.tcx),
                },
            })
            .collect::<Vec<_>>()
    }

    fn get_summary_key_for_callee_info(&self, callee_info: &CalleeInfo<'tcx>) -> SummaryKey<'tcx> {
        // Second part of the key is the calling context (types of callee function's args)
        (
            callee_info.callee_def_id,
            self.get_callee_args_types(callee_info),
        )
    }

    fn overwrite_place_to(&mut self, place_from: &Place, place_to: &Place) {
        let local_info_from = self.domain_state.get_local_info_for_place(place_from);

        if local_info_from == self.domain_state.get_local_info_for_place(place_to) {
            return;
        }

        if let Some(local_info_from) = local_info_from {
            if place_to.projection.is_empty() {
                // The place should already have a good enough type precision
            } else {
                // Reflect the type of the given field of "place_from" to the given field of "place_to"
                if let ProjectionElem::Field(field ,_) = place_to.projection.last().unwrap()
                && self.get_local_info_for_place(place_to).unwrap().has_fields()
                {
                    self.domain_state.locals_info[place_to.local].set_field(*field, local_info_from);
                }  else if let ProjectionElem::Deref = place_to.projection.last().unwrap() {
                    // Reflect the whole reference
                    self.domain_state.locals_info[place_to.local] = local_info_from;
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

                self.domain_state.add_steps(Cost::Scalar(1));
            }
            Rvalue::UnaryOp(_, op) => {
                self.visit_operand(op, location);

                self.domain_state.add_steps(Cost::Scalar(1));
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
                            self.domain_state.locals_info[place_to.local].set_ty(self.tcx.mk_fn_def(def_id, substs_ref));
                        } else if let Some(constant) = operand.constant() {
                            // In case of standard constant, update with its type
                            self.domain_state.locals_info[place_to.local].set_ty(constant.ty());
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
