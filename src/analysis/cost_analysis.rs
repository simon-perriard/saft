use super::cost_domain::CostDomain;
use super::cost_language::{Cost, HasSize, Symbolic};
use super::events_variants_domain::Variants;
use super::pallet::Pallet;
use super::storage_actions::HasAccessType;
use crate::events_variants_domain::EventVariantsDomain;
use crate::storage_actions::AccessType;
use rustc_middle::mir::{
    traversal::*, visit::*, BasicBlock, Body, Local, Location, Operand, Rvalue, Statement,
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

pub(crate) type Summary = HashMap<DefId, Option<CostDomain>>;

pub(crate) struct CostAnalysis<'tcx, 'inter, 'intra> {
    tcx: TyCtxt<'tcx>,
    pallet: &'inter Pallet,
    events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
    body: &'intra Body<'tcx>,
    def_id: DefId,
    local_types: Rc<RefCell<HashMap<Local, Ty<'tcx>>>>,
    pub summaries: Rc<RefCell<Summary>>,
    pub is_success: Rc<RefCell<bool>>,
}

impl<'tcx, 'inter, 'intra> CostAnalysis<'tcx, 'inter, 'intra> {
    pub(crate) fn new(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
        def_id: DefId,
        body: &'intra Body<'tcx>,
    ) -> Self {
        Self::new_with_init(
            tcx,
            pallet,
            events_variants,
            def_id,
            body,
            HashMap::new(),
            Rc::new(RefCell::new(HashMap::new())),
            Rc::new(RefCell::new(true)),
        )
    }

    fn new_with_init(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
        def_id: DefId,
        body: &'intra Body<'tcx>,
        local_types_outter: HashMap<Local, Ty<'tcx>>,
        summaries: Rc<RefCell<Summary>>,
        is_success: Rc<RefCell<bool>>,
    ) -> Self {
        // Fill the map with current body type
        let mut local_types = HashMap::new();

        for (local, local_decl) in body.local_decls.iter_enumerated() {
            local_types.insert(local, local_decl.ty);
        }

        // Replace with more precise types from local_types_outter
        for (local, ty) in local_types_outter.iter() {
            local_types.insert(*local, *ty);
        }

        let local_types = Rc::new(RefCell::new(local_types));

        CostAnalysis {
            tcx,
            pallet,
            events_variants,
            def_id,
            body,
            local_types,
            summaries,
            is_success,
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
            self.body,
            self.local_types.clone(),
            state,
            self.is_success.clone(),
        )
    }
}

pub(crate) struct TransferFunction<'tcx, 'inter, 'intra> {
    tcx: TyCtxt<'tcx>,
    pallet: &'inter Pallet,
    events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
    summaries: Rc<RefCell<Summary>>,
    def_id: DefId,
    body: &'intra Body<'tcx>,
    local_types: Rc<RefCell<HashMap<Local, Ty<'tcx>>>>,
    state: &'intra mut CostDomain,
    is_success: Rc<RefCell<bool>>,
}

impl<'tcx, 'inter, 'intra> TransferFunction<'tcx, 'inter, 'intra> {
    pub fn new(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        events_variants: &'inter HashMap<DefId, EventVariantsDomain>,
        summaries: Rc<RefCell<Summary>>,
        def_id: DefId,
        body: &'intra Body<'tcx>,
        local_types: Rc<RefCell<HashMap<Local, Ty<'tcx>>>>,
        state: &'intra mut CostDomain,
        is_success: Rc<RefCell<bool>>,
    ) -> Self {
        TransferFunction {
            tcx,
            pallet,
            events_variants,
            summaries,
            def_id,
            body,
            local_types,
            state,
            is_success,
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
    ) {
        if let (Some(access_type), cost) = self.is_storage_call(target_def_id, substs) {
            self.analyze_storage_access(substs, args, location, access_type, cost)
        } else {
            self.t_fn_call_analysis(target_def_id, args, location);
        }
    }

    fn t_fn_call_analysis(
        &mut self,
        target_def_id: DefId,
        args: Vec<Operand<'tcx>>,
        location: Location,
    ) {
        if self.summaries.borrow_mut().contains_key(&target_def_id) {
            // We already have the summary for this function, retrieve it and return
            let summaries = self.summaries.borrow_mut();
            let summary = summaries.get(&target_def_id).unwrap();

            if let Some(summary) = summary {
                // Add the cost of calling the target function
                self.state.inter_join(summary);
            } else {
                // we are in a recursive call, just ignore it
                println!("Recursive calls not supported.");
                *self.is_success.borrow_mut() = false;
            }
        } else if self.is_deposit_event(target_def_id) {
            // Filtering event deposit, pallet::deposit_event will later call frame_system::deposit_event,
            // but we can only catch the variant of the Event enum now. Or we need to pass a calling context for
            // further analysis.
            self.analyze_deposit_event(args, location);
        } else if self.tcx.is_mir_available(target_def_id) {
            // We don't have the summary but MIR is available, we need to analyze the function
            self.analyze_with_mir(target_def_id, args);
        } else if self.is_closure_call(target_def_id) {
            self.analyze_closure_call(args, location);
        } else {
            // No MIR available, but symbolically account for the call cost
            self.state.add_steps(Cost::Symbolic(Symbolic::TimeOf(
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
    ) {
        if let TyKind::Closure(closure_def_id, _) = substs.last().unwrap().expect_ty().kind() {
            // Storage access functions may have closures as parameters, we need to analyze them
            self.t_fn_call_analysis(*closure_def_id, args, location);
        }

        // Account for the cost of calling the storage access function
        match access_type {
            AccessType::Read => self.state.add_reads(cost),
            AccessType::Write => self.state.add_writes(cost),
            AccessType::Both => {
                self.state.add_reads(cost.clone());
                self.state.add_writes(cost)
            }
        }
    }

    fn analyze_deposit_event(&mut self, args: Vec<Operand<'tcx>>, location: Location) {
        if let TyKind::Adt(adt_def, _) = args[0].ty(self.body, self.tcx).kind() {
            let event_variants = self
                .events_variants
                .get(&self.def_id)
                .unwrap()
                .get(location)
                .unwrap();

            match event_variants {
                Variants::Variant(variant_id) => {
                    let ty = args[0].ty(self.body, self.tcx);

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

                    self.state.add_events(cost);
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
                    self.state.add_events(cost);
                }
            }
        }
    }

    fn analyze_with_mir(&mut self, target_def_id: DefId, args: Vec<Operand<'tcx>>) {
        // Initialize the summary to None so we can detect a recursive call later
        self.summaries.borrow_mut().insert(target_def_id, None);

        let target_mir = self.tcx.optimized_mir(target_def_id);

        // Detect loops in analyzed function
        if target_mir.is_cfg_cyclic() {
            println!(
                "Loop detected in function {}, loops are not supported",
                self.tcx.def_path_str(target_def_id)
            );
            *self.is_success.borrow_mut() = false;
            return;
        }

        let mut args_ty = HashMap::new();
        // Local 0_ will be return value, args start at 1_
        let mut arg_count = 1;
        for arg in args {
            match &arg {
                Operand::Copy(place) | Operand::Move(place) => {
                    args_ty.insert(
                        Local::from_u32(arg_count),
                        *self.local_types.borrow().get(&place.local).unwrap(),
                    );
                }
                Operand::Constant(constant) => {
                    if let Some((def_id, substs_ref)) = arg.const_fn_def() {
                        args_ty.insert(Local::from_u32(arg_count), self.tcx.type_of(def_id));
                    } else {
                        args_ty.insert(Local::from_u32(arg_count), constant.ty());
                    }
                }
            };
            arg_count += 1;
        }

        // Analyze the target function
        let mut results = CostAnalysis::new_with_init(
            self.tcx,
            self.pallet,
            self.events_variants,
            target_def_id,
            target_mir,
            args_ty,
            self.summaries.clone(),
            self.is_success.clone(),
        )
        .into_engine(self.tcx, target_mir)
        .pass_name("cost_analysis")
        .iterate_to_fixpoint()
        .into_results_cursor(target_mir);

        // Retrieve target function analysis success flag
        let fn_call_success_anaylsis_state = *results.analysis().is_success.borrow();
        let self_success_state = *self.is_success.borrow();
        *self.is_success.borrow_mut() = self_success_state && fn_call_success_anaylsis_state;

        // Retrieve last state of callee function as its summary
        let end_state = if let Some((last, _)) = reverse_postorder(target_mir).last() {
            results.seek_to_block_end(last);
            Some(results.get().clone())
        } else {
            None
        };

        if let Some(end_state) = end_state {
            // Update caller function state
            self.state.inter_join(&end_state);

            // Add the callee function summary to our summaries map
            self.summaries
                .borrow_mut()
                .insert(target_def_id, Some(end_state));
        }
    }

    fn analyze_closure_call(&mut self, args: Vec<Operand<'tcx>>, location: Location) {
        // First arg is closure
        //println!("{:?}", self.local_types.borrow().get(&args[0].place().unwrap().local).unwrap().kind());
        let target_def_id = match self
            .local_types
            .borrow()
            .get(&args[0].place().unwrap().local)
            .unwrap()
            .kind()
        {
            TyKind::Closure(closure_def_id, _) => {
                //println!("{:?}", param_ty);
                Some(closure_def_id)
            }
            _ => None, //TODO
        };

        if let Some(target_def_id) = target_def_id {
            self.t_fn_call_analysis(*target_def_id, args, location);
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

impl<'intra, 'tcx> Visitor<'tcx> for TransferFunction<'tcx, '_, '_> {
    fn visit_rvalue(&mut self, rvalue: &Rvalue<'tcx>, location: Location) {
        match rvalue {
            Rvalue::BinaryOp(_, box (lhs, rhs)) | Rvalue::CheckedBinaryOp(_, box (lhs, rhs)) => {
                self.visit_operand(lhs, location);
                self.visit_operand(rhs, location);

                self.state.add_steps(Cost::Concrete(1));
            }
            Rvalue::UnaryOp(_, op) => {
                self.visit_operand(op, location);

                self.state.add_steps(Cost::Concrete(1));
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
                let place_from_ty = self.local_types.borrow().get(&place_from.local).unwrap().clone();
                self.local_types.borrow_mut().insert(place_to.local, place_from_ty);
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
                ..
            } if let TyKind::FnDef(target_def_id, substs) = c.ty().kind() => {
                self.visit_source_info(source_info);

                for arg in args.iter() {
                    self.visit_operand(arg, location);
                }

                self.t_visit_fn_call(*target_def_id, substs, (*args).clone(), location);
            }
            _ => self.super_terminator(terminator, location),
        }
    }
}

impl<'inter> AnalysisDomain<'inter> for CostAnalysis<'inter, '_, '_> {
    type Domain = CostDomain;
    const NAME: &'static str = "CostAnalysis";

    type Direction = Forward;

    fn bottom_value(&self, _body: &Body<'inter>) -> Self::Domain {
        CostDomain::new()
    }

    fn initialize_start_block(&self, _body: &Body<'inter>, _state: &mut Self::Domain) {}
}

impl<'tcx> Analysis<'tcx> for CostAnalysis<'tcx, '_, '_> {
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
