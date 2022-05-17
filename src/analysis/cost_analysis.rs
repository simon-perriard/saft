use super::cost_domain::CostDomain;
use super::cost_language::{Cost, HasSize, Symbolic};
use super::events_variants_domain::Variants;
use super::pallet::Pallet;
use super::storage_actions::HasAccessType;
use crate::analysis::events_variants_domain::EventVariantsDomain;
use crate::analysis::storage_actions::AccessType;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{
    traversal::*, visit::*, BasicBlock, Body, Local, Location, Operand, Place, Rvalue, Statement,
    StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::Ty;
use rustc_middle::ty::{subst::SubstsRef, TyCtxt, TyKind};
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
    local_types: Rc<RefCell<LocalTypes<'tcx>>>,
    domain_state: &'intra mut CostDomain,
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
    fn t_visit_fn_call(
        &mut self,
        target_def_id: DefId,
        substs: &'tcx SubstsRef,
        args: Vec<Operand<'tcx>>,
        location: Location,
        destination: Option<(Place<'tcx>, BasicBlock)>,
    ) {
        if let (Some(access_type), cost) = self.is_storage_call(target_def_id, substs) {
            self.analyze_storage_access(substs, args, location, access_type, cost, destination);
        } else {
            self.t_fn_call_analysis(target_def_id, args, location, destination);
        }
    }

    fn t_fn_call_analysis(
        &mut self,
        target_def_id: DefId,
        args: Vec<Operand<'tcx>>,
        location: Location,
        destination: Option<(Place<'tcx>, BasicBlock)>,
    ) {
        if self
            .summaries
            .borrow_mut()
            .contains_key(&(target_def_id, (*self.local_types.borrow()).clone()))
        {
            // We already have the summary for this function, retrieve it and return
            let summaries = self.summaries.borrow_mut();
            let summary = summaries
                .get(&(target_def_id, (*self.local_types.borrow()).clone()))
                .unwrap();

            if let Some(summary) = summary {
                // Add the cost of calling the target function
                self.domain_state.inter_join(summary);
            } else {
                // we are in a recursive call, just ignore it
                println!("Recursive calls not supported.");
                *self.analysis_state.borrow_mut() = AnalysisState::Failure;
            }
        } else if self.is_deposit_event(target_def_id) {
            // Filtering event deposit, pallet::deposit_event will later call frame_system::deposit_event,
            // but we can only catch the variant of the Event enum now. Or we need to pass a calling context for
            // further analysis.
            self.analyze_deposit_event(args, location);
        } else if self.tcx.is_mir_available(target_def_id) {
            // We don't have the summary but MIR is available, we need to analyze the function
            self.analyze_with_mir(target_def_id, args, destination);
        } else if self.is_closure_call(target_def_id) {
            self.analyze_closure_call(args, location, destination);
        } else {
            // No MIR available, but symbolically account for the call cost
            self.domain_state.add_steps(Cost::Symbolic(Symbolic::TimeOf(
                self.tcx.def_path_str(target_def_id),
            )));
        }
    }

    fn analyze_storage_access(
        &mut self,
        substs: &'tcx SubstsRef,
        args: Vec<Operand<'tcx>>,
        location: Location,
        access_type: AccessType,
        cost: Cost,
        destination: Option<(Place<'tcx>, BasicBlock)>,
    ) {
        if let TyKind::Closure(closure_def_id, _) = substs.last().unwrap().expect_ty().kind() {
            // Storage access functions may have closures as parameters, we need to analyze them
            self.t_fn_call_analysis(*closure_def_id, args, location, destination);
        }

        // Account for the cost of calling the storage access function
        match access_type {
            AccessType::Read => self.domain_state.add_reads(cost),
            AccessType::Write => self.domain_state.add_writes(cost),
            AccessType::Both => {
                self.domain_state.add_reads(cost.clone());
                self.domain_state.add_writes(cost)
            }
        }
    }

    fn analyze_deposit_event(&mut self, args: Vec<Operand<'tcx>>, location: Location) {
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

    fn analyze_with_mir(
        &mut self,
        target_def_id: DefId,
        args: Vec<Operand<'tcx>>,
        destination: Option<(Place<'tcx>, BasicBlock)>,
    ) {
        // Initialize the summary to None so we can detect a recursive call later
        self.summaries
            .borrow_mut()
            .insert((target_def_id, (*self.local_types.borrow()).clone()), None);

        let target_mir = self.tcx.optimized_mir(target_def_id);

        // Detect loops in analyzed function
        if target_mir.is_cfg_cyclic() {
            println!(
                "Loop detected in function {}, loops are not supported",
                self.tcx.def_path_str(target_def_id)
            );
            *self.analysis_state.borrow_mut() = AnalysisState::Failure;
            return;
        }

        let mut local_types_outter = LocalTypes::new();
        // Local 0_ will be return value, args start at 1_
        if let Some((place_to, _)) = destination {
            local_types_outter.push(self.local_types.borrow()[place_to.local]);
        } else {
            // If none, the call necessarily diverges.
            // cf. https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/terminator/enum.TerminatorKind.html#variant.Call
        }

        for arg in args {
            match &arg {
                Operand::Copy(place) | Operand::Move(place) => {
                    local_types_outter.push(*self.local_types.borrow().get(place.local).unwrap());
                }
                Operand::Constant(constant) => {
                    if let Some((def_id, substs_ref)) = arg.const_fn_def() {
                        local_types_outter.push(self.tcx.type_of(def_id));
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
            target_def_id,
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
            None
        };

        if let Some(end_state) = end_state {
            // Update caller function state
            self.domain_state.inter_join(&end_state);

            // Add the callee function summary to our summaries map
            self.summaries.borrow_mut().insert(
                (target_def_id, (*self.local_types.borrow()).clone()),
                Some(end_state),
            );
        }
    }

    fn analyze_closure_call(
        &mut self,
        args: Vec<Operand<'tcx>>,
        location: Location,
        destination: Option<(Place<'tcx>, BasicBlock)>,
    ) {
        // First arg is closure or action
        let target_def_id = match self
            .local_types
            .borrow()
            .get(args[0].place().unwrap().local)
            .unwrap()
            .kind()
        {
            TyKind::Closure(closure_def_id, _) => Some(closure_def_id),
            TyKind::FnDef(def_id, _) => Some(def_id),
            TyKind::Ref(_, _, _) => None,
            TyKind::Projection(_) => None,
            _ => unreachable!(),
        };

        if let Some(target_def_id) = target_def_id {
            let mut closure_args = args.clone();
            // First arg is closure, we remove it to keep only its args
            closure_args.pop();
            self.t_fn_call_analysis(*target_def_id, closure_args, location, destination);
        }
    }

    fn is_storage_call(
        &self,
        def_id: DefId,
        substs: &'tcx SubstsRef,
    ) -> (Option<AccessType>, Cost) {
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
                        return (
                            field.get_access_type(&tcx.def_path_str(def_id)),
                            field.get_size(&tcx),
                        );
                    }
                }
            }
        }
        (None, Cost::default())
    }

    fn is_deposit_event(&self, target_def_id: DefId) -> bool {
        let path = self.tcx.def_path_str(target_def_id);
        path.starts_with("pallet::Pallet") && path.ends_with("deposit_event")
    }

    fn is_closure_call(&self, target_def_id: DefId) -> bool {
        let path: &str = &self.tcx.def_path_str(target_def_id);
        let closure_calls_list = vec![
            "std::ops::FnOnce::call_once",
            "std::ops::Fn::call",
            "std::ops::FnMut::call_mut",
        ];

        closure_calls_list.contains(&path)
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
            StatementKind::Assign(box (place_to, rvalue)) if let Rvalue::Use(operand) = rvalue && let Some(place_from) = operand.place() => {
                self.visit_source_info(source_info);

                // In case of move of copy, update with the most precise type
                let place_from_ty = *self.local_types.borrow().get(place_from.local).unwrap();
                self.local_types.borrow_mut()[place_to.local] = place_from_ty;
            },
            _ => self.super_statement(statement, location),
        }
    }

    fn visit_terminator(&mut self, terminator: &Terminator<'tcx>, location: Location) {
        let Terminator { source_info, kind } = terminator;

        match kind {
            TerminatorKind::Call {
                func: Operand::Constant(c),
                args,
                destination,
                ..
            } if let TyKind::FnDef(target_def_id, substs) = c.ty().kind() => {
                self.visit_source_info(source_info);

                for arg in args.iter() {
                    self.visit_operand(arg, location);
                }

                self.t_visit_fn_call(*target_def_id, substs, (*args).clone(), location, *destination);
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
        self.transfer_function(state)
            .visit_statement(statement, location)
    }

    fn apply_terminator_effect(
        &self,
        state: &mut Self::Domain,
        terminator: &Terminator<'tcx>,
        location: Location,
    ) {
        self.transfer_function(state)
            .visit_terminator(terminator, location);
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
