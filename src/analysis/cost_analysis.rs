use super::cost_domain::CostDomain;
use super::cost_language::{Cost, Symbolic};
use super::events_variants_domain::Variants;
use super::pallet::Pallet;
use super::specifications::storage_actions_specs::{HasAccessCost, AccessCost};
use crate::analysis::events_variants_domain::EventVariantsDomain;
use crate::analysis::specifications::dispatch_to_specifications;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{
    traversal::*, visit::*, BasicBlock, Body, Local, Location, Operand, Place, ProjectionElem,
    Rvalue, Statement, StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::{subst::SubstsRef, Ty, TyCtxt, TyKind};
use rustc_mir_dataflow::{Analysis, AnalysisDomain, CallReturnPlaces, Forward};
use rustc_span::def_id::DefId;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::vec;

pub(crate) type LocalTypes<'tcx> = IndexVec<Local, Ty<'tcx>>;
pub(crate) type Summary<'tcx> = HashMap<(DefId, LocalTypes<'tcx>), Option<CostDomain>>;

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

pub(crate) struct CalleeInfo<'tcx> {
    pub location: Location,
    pub func: Operand<'tcx>,
    pub args: Vec<Operand<'tcx>>,
    pub destination: Option<(Place<'tcx>, BasicBlock)>,
    pub callee_def_id: DefId,
    pub substs_ref: SubstsRef<'tcx>,
}

pub(crate) struct CostAnalysis<'tcx, 'inter> {
    tcx: TyCtxt<'tcx>,
    pallet: &'inter Pallet,
    events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
    def_id: DefId,
    local_types: Rc<RefCell<LocalTypes<'tcx>>>,
    pub summaries: Rc<RefCell<Summary<'tcx>>>,
    pub analysis_state: Rc<RefCell<AnalysisState>>,
}

impl<'tcx, 'inter, 'intra> CostAnalysis<'tcx, 'inter> {
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
            LocalTypes::new(),
            Rc::new(RefCell::new(HashMap::new())),
            Rc::new(RefCell::new(AnalysisState::Success)),
        )
    }

    fn new_with_init(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
        def_id: DefId,
        local_types_outter: LocalTypes<'tcx>,
        summaries: Rc<RefCell<Summary<'tcx>>>,
        state: Rc<RefCell<AnalysisState>>,
    ) -> Self {
        // Fill the map with current body type
        let body = tcx.optimized_mir(def_id);

        let mut local_types: LocalTypes<'tcx> = body
            .local_decls
            .iter()
            .map(|local_decl| local_decl.ty)
            .collect();

        // Replace with more precise types from local_types_outter
        for (local, ty) in local_types_outter.iter_enumerated() {
            local_types[local] = *ty;
        }

        let local_types = Rc::new(RefCell::new(local_types));

        CostAnalysis {
            tcx,
            pallet,
            events_variants,
            def_id,
            local_types,
            summaries,
            analysis_state: state,
        }
    }

    pub(crate) fn transfer_function(
        &self,
        state: &'intra mut CostDomain,
    ) -> TransferFunction<'tcx, 'inter, 'intra> {
        TransferFunction::new(
            self.tcx,
            self.pallet,
            self.events_variants,
            self.summaries.clone(),
            self.def_id,
            self.local_types.clone(),
            state,
            self.analysis_state.clone(),
        )
    }
}

pub(crate) struct TransferFunction<'tcx, 'inter, 'intra> {
    tcx: TyCtxt<'tcx>,
    pallet: &'inter Pallet,
    events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
    summaries: Rc<RefCell<Summary<'tcx>>>,
    def_id: DefId,
    pub local_types: Rc<RefCell<LocalTypes<'tcx>>>,
    pub domain_state: &'intra mut CostDomain,
    analysis_state: Rc<RefCell<AnalysisState>>,
}

impl<'tcx, 'inter, 'intra> TransferFunction<'tcx, 'inter, 'intra> {
    pub fn new(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
        summaries: Rc<RefCell<Summary<'tcx>>>,
        def_id: DefId,
        local_types: Rc<RefCell<LocalTypes<'tcx>>>,
        domain_state: &'intra mut CostDomain,
        analysis_state: Rc<RefCell<AnalysisState>>,
    ) -> Self {
        TransferFunction {
            tcx,
            pallet,
            events_variants,
            summaries,
            def_id,
            local_types,
            domain_state,
            analysis_state,
        }
    }
}

impl<'visitor, 'tcx> TransferFunction<'tcx, '_, '_>
where
    Self: Visitor<'visitor>,
{
    fn t_visit_fn_call(&mut self, location: Location) {
        let callee_info = self.get_callee_info(location);

        if let Some(access_cost) =
            self.is_storage_call(callee_info.callee_def_id, callee_info.substs_ref)
        {
            // Filtering storage access, we need to catch it now otherwise we lose information about which
            // field is accessed.
            self.analyze_storage_access(callee_info, access_cost);
        } else if self.is_deposit_event(callee_info.callee_def_id) {
            // Filtering event deposit, pallet::deposit_event will later call frame_system::deposit_event,
            // but we can only catch the variant of the Event enum now. Or we need to pass a calling context for
            // further analysis.
            self.analyze_deposit_event(callee_info);
        } else {
            self.t_fn_call_analysis(callee_info);
        }
    }

    fn t_fn_call_analysis(&mut self, callee_info: CalleeInfo<'tcx>) {
        if self.summaries.borrow_mut().contains_key(&(
            callee_info.callee_def_id,
            (*self.local_types.borrow()).clone(),
        )) {
            // We already have the summary for this function, retrieve it and return
            let summaries = self.summaries.borrow_mut();
            let summary = summaries
                .get(&(
                    callee_info.callee_def_id,
                    (*self.local_types.borrow()).clone(),
                ))
                .unwrap();

            if let Some(summary) = summary {
                // Add the cost of calling the target function
                self.domain_state.inter_join(summary);
            } else {
                // we are in a recursive call, just ignore it
                println!("Recursive calls not supported.");
                *self.analysis_state.borrow_mut() = AnalysisState::Failure;
            }
        } else if self.tcx.is_mir_available(callee_info.callee_def_id) {
            // We don't have the summary but MIR is available, we need to analyze the function
            self.analyze_with_available_mir(callee_info);
        } else if self.is_closure_call(callee_info.callee_def_id) {
            self.analyze_closure_call(callee_info);
        } else {
            // No MIR available, but symbolically account for the call cost
            dispatch_to_specifications(self.tcx, self, callee_info);
        }
    }

    fn analyze_storage_access(
        &mut self,
        callee_info: CalleeInfo<'tcx>,
        access_cost: AccessCost,
    ) {
        if let TyKind::Closure(closure_def_id, _) =
            callee_info.substs_ref.last().unwrap().expect_ty().kind()
        {
            // Storage access functions may have closures as parameters, we need to analyze them
            let closure_info = CalleeInfo {
                callee_def_id: *closure_def_id,
                // precise enough args type will be inferred by the closure body
                args: vec![],
                // cannot keep the storage access return type as it is not the same as closure's
                // precise enough args type will be inferred by the closure body
                destination: None,
                ..callee_info
            };
            self.t_fn_call_analysis(closure_info);
        }

        // Account for the cost of calling the storage access function
        self.domain_state.add_reads(access_cost.reads);
        self.domain_state.add_writes(access_cost.writes);
        self.domain_state.add_steps(access_cost.steps);
    }

    fn analyze_deposit_event(&mut self, callee_info: CalleeInfo<'tcx>) {
        // Account for function call
        self.domain_state.add_steps(Cost::Concrete(1));

        let args = callee_info.args;
        let location = callee_info.location;

        let body = self.tcx.optimized_mir(self.def_id);

        if let TyKind::Adt(adt_def, _) = args[0].ty(body, self.tcx).kind() {
            let event_variants = self
                .events_variants
                .get(&self.def_id)
                .unwrap()
                .get(location)
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
                                    Cost::Concrete(ty_and_layout.layout.size().bytes())
                                }
                                rustc_target::abi::Variants::Multiple { variants, .. } => {
                                    let variant_layout = variants[*variant_id];
                                    Cost::Concrete(variant_layout.size().bytes())
                                }
                            }
                        }
                        Err(_) => {
                            let variant = adt_def.variant(*variant_id);
                            Cost::Symbolic(Symbolic::SizeOf(self.tcx.def_path_str(variant.def_id)))
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
                            // For now add the variant size as symbolic
                            Cost::Symbolic(Symbolic::SizeOf(self.tcx.def_path_str(variant.def_id)))
                        })
                        .reduce(|accum, item| accum.max(&item))
                        .unwrap();
                    self.domain_state.add_events(cost);
                }
            }
        }
    }

    fn analyze_with_available_mir(&mut self, callee_info: CalleeInfo<'tcx>) {
        // Initialize the summary to None so we can detect a recursive call later
        self.summaries.borrow_mut().insert(
            (
                callee_info.callee_def_id,
                (*self.local_types.borrow()).clone(),
            ),
            None,
        );

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

    fn analyze_with_mir(&mut self, callee_info: CalleeInfo<'tcx>, target_mir: &Body<'tcx>) {
        // Account for function call
        self.domain_state.add_steps(Cost::Concrete(1));

        let mut local_types_outter = LocalTypes::new();
        // Local 0_ will be return value, args start at 1_
        if let Some((place_to, _)) = callee_info.destination {
            local_types_outter.push(self.local_types.borrow()[place_to.local]);
        } else {
            // If none, the call necessarily diverges.
            // cf. https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/terminator/enum.TerminatorKind.html#variant.Call
        }

        for arg in callee_info.args {
            match &arg {
                Operand::Copy(place) | Operand::Move(place) => {
                    local_types_outter.push(*self.local_types.borrow().get(place.local).unwrap());
                }
                Operand::Constant(constant) => {
                    if let Some((def_id, substs_ref)) = arg.const_fn_def() {
                        // Constant function,
                        // cf. https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/enum.Operand.html#method.const_fn_def
                        local_types_outter.push(self.tcx.mk_fn_def(def_id, substs_ref));
                    } else {
                        local_types_outter.push(constant.ty());
                    }
                }
            };
        }

        // Analyze the target function
        let mut results = CostAnalysis::new_with_init(
            self.tcx,
            self.pallet,
            self.events_variants,
            callee_info.callee_def_id,
            local_types_outter,
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
            // Update caller function state
            self.domain_state.inter_join(&end_state);

            // Add the callee function summary to our summaries map
            self.summaries.borrow_mut().insert(
                (
                    callee_info.callee_def_id,
                    (*self.local_types.borrow()).clone(),
                ),
                Some(end_state),
            );
        }
    }

    fn analyze_closure_call(&mut self, callee_info: CalleeInfo<'tcx>) {
        // First arg is closure or action
        let callee_def_id = match self
            .local_types
            .borrow()
            .get(callee_info.args[0].place().unwrap().local)
            .unwrap()
            .kind()
        {
            TyKind::Closure(closure_def_id, _) => Some(*closure_def_id),
            TyKind::FnDef(def_id, _) => Some(*def_id),
            TyKind::Ref(_, _, _) => None,
            TyKind::Projection(_) => None,
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
            self.t_fn_call_analysis(closure_info);
        }
    }

    fn is_storage_call(
        &self,
        def_id: DefId,
        substs: SubstsRef<'tcx>,
    ) -> Option<AccessCost> {
        if self
            .tcx
            .def_path_str(def_id)
            .starts_with("frame_support::pallet_prelude::Storage")
        {
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
                for (ty, field) in pallet
                    .fields
                    .iter()
                    .map(|(field_def_id, field)| (tcx.type_of(field_def_id), field))
                {
                    
                    if ty == reconstructed_ty {
                        return field.get_access_cost(tcx, &tcx.def_path_str(def_id));
                    }
                }
            }
        }
        None
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
                 location,
                 func: (*func).clone(),
                 args: (*args).clone(),
                 destination: *destination,
                 callee_def_id,
                 substs_ref
             },
            _ => unreachable!("{:?}", terminator.kind),
        }
    }
}

impl<'tcx> Visitor<'tcx> for TransferFunction<'tcx, '_, '_> {
    fn visit_rvalue(&mut self, rvalue: &Rvalue<'tcx>, location: Location) {
        match rvalue {
            Rvalue::BinaryOp(_, box (lhs, rhs)) | Rvalue::CheckedBinaryOp(_, box (lhs, rhs)) => {
                self.visit_operand(lhs, location);
                self.visit_operand(rhs, location);

                self.domain_state.add_steps(Cost::Concrete(1));
            }
            Rvalue::UnaryOp(_, op) => {
                self.visit_operand(op, location);

                self.domain_state.add_steps(Cost::Concrete(1));
            }
            _ => self.super_rvalue(rvalue, location),
        }
    }

    fn visit_statement(&mut self, statement: &Statement<'tcx>, location: Location) {
        let Statement { source_info, kind } = statement;

        match kind {
            StatementKind::Assign(box (place_to, rvalue)) if let Rvalue::Use(operand) = rvalue && place_to.projection.is_empty() => {
                self.visit_source_info(source_info);

                match operand {
                    Operand::Copy(place_from) |
                    Operand::Move(place_from) => {

                        // In case of move of copy, update with the most precise type from our type map

                        if place_from.projection.is_empty() {
                            // Move or copy the whole place
                            let place_from_ty = *self.local_types.borrow().get(place_from.local).unwrap();
                            self.local_types.borrow_mut()[place_to.local] = place_from_ty;
                        } else {
                            // We consider the last projection only
                            //      cf. https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/struct.Place.html
                            // and we only care if it's a field
                            if let ProjectionElem::Field(_, ty) = place_from.projection.last().unwrap() {
                                self.local_types.borrow_mut()[place_to.local] = *ty;
                            }
                        }
                    },
                    Operand::Constant(_) => {
                        if let Some((def_id, substs_ref)) = operand.const_fn_def() {
                            // In case of constant function, update with the function type
                            self.local_types.borrow_mut()[place_to.local] = self.tcx.mk_fn_def(def_id, substs_ref);
                        } else if let Some(constant) = operand.constant() {
                            // In case of standard constant, update with its type
                            self.local_types.borrow_mut()[place_to.local] = constant.ty();
                        }
                    }
                }
            },
            _ => self.super_statement(statement, location),
        }
    }

    fn visit_terminator(&mut self, terminator: &Terminator<'tcx>, location: Location) {
        let Terminator { source_info, kind } = terminator;

        match kind {
            TerminatorKind::Call {
                func: Operand::Constant(c),
                ..
            } if let TyKind::FnDef(target_def_id, substs) = c.ty().kind() => {
                self.visit_source_info(source_info);

                self.t_visit_fn_call(location);
            }
            _ => self.super_terminator(terminator, location),
        }
    }
}

impl<'inter> AnalysisDomain<'inter> for CostAnalysis<'inter, '_> {
    type Domain = CostDomain;
    const NAME: &'static str = "CostAnalysis";

    type Direction = Forward;

    fn bottom_value(&self, _body: &Body<'inter>) -> Self::Domain {
        CostDomain::new()
    }

    fn initialize_start_block(&self, _body: &Body<'inter>, _state: &mut Self::Domain) {}
}

impl<'tcx> Analysis<'tcx> for CostAnalysis<'tcx, '_> {
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
