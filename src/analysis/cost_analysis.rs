use super::pallet::Pallet;
use super::cost_domain::CostDomain;
use super::size_language::{HasSize, Size};
use super::storage_actions::HasAccessType;
use super::time_language::Time;
use crate::storage_actions::AccessType;
use rpds::HashTrieMap;
use rustc_middle::mir::{
    traversal::*, visit::*, BasicBlock, BinOp, Body, Location, Operand, Rvalue, Statement,
    StatementKind, Terminator, TerminatorKind, UnOp,
};
use rustc_middle::ty::{subst::SubstsRef, TyCtxt, TyKind};
use rustc_mir_dataflow::{Analysis, AnalysisDomain, CallReturnPlaces, Forward};
use rustc_span::def_id::DefId;
use std::cell::RefCell;
use std::rc::Rc;

pub(crate) type Summary = HashTrieMap<DefId, Option<CostDomain>>;

pub(crate) struct RWCountAnalysis<'tcx, 'inter, 'intra> {
    tcx: TyCtxt<'tcx>,
    pallet: &'inter Pallet,
    body: &'intra Body<'tcx>,
    def_id: DefId,
    pub summaries: Rc<RefCell<Summary>>,
    pub is_success: Rc<RefCell<bool>>,
}

impl<'tcx, 'inter, 'intra> RWCountAnalysis<'tcx, 'inter, 'intra> {
    pub(crate) fn new(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        def_id: DefId,
        body: &'intra Body<'tcx>,
    ) -> Self {
        Self::new_with_init(
            tcx,
            pallet,
            def_id,
            body,
            Rc::new(RefCell::new(HashTrieMap::new())),
            Rc::new(RefCell::new(true)),
        )
    }

    fn new_with_init(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        def_id: DefId,
        body: &'intra Body<'tcx>,
        summaries: Rc<RefCell<Summary>>,
        is_success: Rc<RefCell<bool>>,
    ) -> Self {
        RWCountAnalysis {
            tcx,
            pallet,
            def_id,
            body,
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
            self.summaries.clone(),
            self.def_id,
            self.body,
            state,
            self.is_success.clone(),
        )
    }
}

pub(crate) struct TransferFunction<'tcx, 'inter, 'intra> {
    tcx: TyCtxt<'tcx>,
    pallet: &'inter Pallet,
    summaries: Rc<RefCell<Summary>>,
    _def_id: DefId,
    body: &'intra Body<'tcx>,
    state: &'intra mut CostDomain,
    is_success: Rc<RefCell<bool>>,
}

impl<'tcx, 'inter, 'intra> TransferFunction<'tcx, 'inter, 'intra> {
    pub fn new(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        summaries: Rc<RefCell<Summary>>,
        _def_id: DefId,
        body: &'intra Body<'tcx>,
        state: &'intra mut CostDomain,
        is_success: Rc<RefCell<bool>>,
    ) -> Self {
        TransferFunction {
            tcx,
            pallet,
            summaries,
            _def_id,
            body,
            state,
            is_success,
        }
    }
}

impl<'visitor, 'tcx> TransferFunction<'tcx, '_, '_>
where
    Self: Visitor<'visitor>,
{
    fn is_storage_call(
        &self,
        def_id: DefId,
        substs: &'tcx SubstsRef,
    ) -> (Option<AccessType>, Size) {
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
        (None, Size::unit())
    }

    fn t_visit_fn_call(
        &mut self,
        target_def_id: DefId,
        substs: &'tcx SubstsRef,
        args: Vec<Operand<'tcx>>,
    ) {
        if let (Some(access_type), size) = self.is_storage_call(target_def_id, substs) {
            if let TyKind::Closure(closure_def_id, _) = substs.last().unwrap().expect_ty().kind() {
                // Storage access functions may have closures as parameters, we need to analyze them
                self.t_fn_call_analysis(*closure_def_id, args);
            }

            // Account for the cost of calling the storage access function
            match access_type {
                AccessType::Read => self.state.add_reads(size),
                AccessType::Write => self.state.add_writes(size),
                AccessType::Both => {
                    self.state.add_reads(size.clone());
                    self.state.add_writes(size)
                }
            }
        } else {
            self.t_fn_call_analysis(target_def_id, args);
        }
    }

    fn t_fn_call_analysis(&mut self, target_def_id: DefId, args: Vec<Operand<'tcx>>) {
        if self.summaries.borrow_mut().contains_key(&target_def_id) {
            // We already have the summary for this function, retrieve it and return
            let summary = self.summaries.borrow_mut();
            let summary = summary.get(&target_def_id).unwrap();

            if let Some(summary) = summary {
                // Add the cost of calling the target function
                self.state.inter_join(summary);
            } else {
                // we are in a recursive call, just ignore it
                println!(
                    "{:?} calling {:?}",
                    self.tcx.def_path_str(self._def_id),
                    self.tcx.def_path_str(target_def_id)
                );
                println!("Recursive calls not supported.");
                *self.is_success.borrow_mut() = false;
            }

            return;
        }

        // Filtering event deposit, pallet::deposit_event will later call frame_system::deposit_event,
        // but we can only catch the variant of the Event enum now. Or we need to pass a calling context for
        // further analysis.
        let path = self.tcx.def_path_str(target_def_id);
        if path.starts_with("pallet::Pallet") && path.ends_with("deposit_event") {
            if let TyKind::Adt(adt_def, _) = args[0].ty(self.body, self.tcx).kind() {
                let variant = adt_def.variant(
                    *self
                        .state
                        .bb_set_discriminant
                        .get(&args[0].place().unwrap().local)
                        .unwrap(),
                );

                // For now add the variant size as symbolic
                let cost = Size::symbolic(self.tcx.def_path_str(variant.def_id), false);

                self.state.add_events(cost);
            }
            return;
        }

        if self.tcx.is_mir_available(target_def_id) {
            // We don't have the summary, we need to analyze the function

            // Initialize the summary to None so we can detect a recursive call later
            self.summaries.borrow_mut().insert_mut(target_def_id, None);

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

            // Analyze the target function
            let mut results = RWCountAnalysis::new_with_init(
                self.tcx,
                self.pallet,
                target_def_id,
                target_mir,
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
                    .insert_mut(target_def_id, Some(end_state));
            }
        } else {
            let path = self.tcx.def_path_str(target_def_id);
            if path.starts_with("std")
                || path.starts_with("alloc")
                || path.starts_with("frame_system") && path.ends_with("ensure_signed")
                || path.starts_with("frame_support") && path.ends_with("ensure_origin")
                || path.starts_with("weights")
            {
                /* No MIR available for those but:
                    - standard library does not do storage access
                    - ensure_signed/origin does not do storage access
                    - weights do not do storage access
                **/
            } else {
                /*println!(
                    "NO MIR AVAILABLE FOR {:?}",
                    self.tcx.def_path_str(target_def_id)
                );*/
            }
        }
    }
}

impl<'intra, 'tcx> Visitor<'tcx> for TransferFunction<'tcx, '_, '_> {
    fn visit_rvalue(&mut self, rvalue: &Rvalue<'tcx>, location: Location) {
        match rvalue {
            Rvalue::BinaryOp(bin_op, box (lhs, rhs))
            | Rvalue::CheckedBinaryOp(bin_op, box (lhs, rhs)) => {
                self.visit_operand(lhs, location);
                self.visit_operand(rhs, location);

                match bin_op {
                    BinOp::Add => self.state.add_time(Time::Symbolic("Add".to_string())),
                    BinOp::Sub => self.state.add_time(Time::Symbolic("Sub".to_string())),
                    BinOp::Mul => self.state.add_time(Time::Symbolic("Mul".to_string())),
                    BinOp::Div => self.state.add_time(Time::Symbolic("Div".to_string())),
                    BinOp::Rem => self.state.add_time(Time::Symbolic("Rem".to_string())),
                    BinOp::BitXor => self.state.add_time(Time::Symbolic("BitXor".to_string())),
                    BinOp::BitAnd => self.state.add_time(Time::Symbolic("BitAnd".to_string())),
                    BinOp::BitOr => self.state.add_time(Time::Symbolic("BitOr".to_string())),
                    BinOp::Shl => self.state.add_time(Time::Symbolic("Shl".to_string())),
                    BinOp::Shr => self.state.add_time(Time::Symbolic("Shr".to_string())),
                    BinOp::Eq => self.state.add_time(Time::Symbolic("Eq".to_string())),
                    BinOp::Lt => self.state.add_time(Time::Symbolic("Lt".to_string())),
                    BinOp::Le => self.state.add_time(Time::Symbolic("Le".to_string())),
                    BinOp::Ne => self.state.add_time(Time::Symbolic("Ne".to_string())),
                    BinOp::Ge => self.state.add_time(Time::Symbolic("Ge".to_string())),
                    BinOp::Gt => self.state.add_time(Time::Symbolic("Gt".to_string())),
                    BinOp::Offset => self.state.add_time(Time::Symbolic("Offset".to_string())),
                }
            }
            Rvalue::UnaryOp(un_op, op) => {
                self.visit_operand(op, location);

                match un_op {
                    UnOp::Not => self.state.add_time(Time::symbolic("Not".to_string())),
                    UnOp::Neg => self.state.add_time(Time::symbolic("Neg".to_string())),
                }
            }
            _ => self.super_rvalue(rvalue, location),
        }
    }

    fn visit_statement(&mut self, statement: &Statement<'tcx>, location: Location) {
        let Statement { source_info, kind } = statement;
        match kind {
            StatementKind::SetDiscriminant {
                place,
                variant_index,
            } => {
                self.visit_source_info(source_info);

                // Set the discriminant for the local, we need it to track
                // the Event enum variant that will be deposited
                self.state
                    .bb_set_discriminant
                    .insert_mut(place.local, *variant_index);
            }
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

                self.t_visit_fn_call(*target_def_id, substs, (*args).clone());
            }
            _ => self.super_terminator(terminator, location),
        }

        // clean basic block discriminants state for next bb
        self.state.reset_bb_discriminants()
    }
}

impl<'inter> AnalysisDomain<'inter> for RWCountAnalysis<'inter, '_, '_> {
    type Domain = CostDomain;
    const NAME: &'static str = "CostAnalysis";

    type Direction = Forward;

    fn bottom_value(&self, _body: &Body<'inter>) -> Self::Domain {
        CostDomain::new()
    }

    fn initialize_start_block(&self, _body: &Body<'inter>, _state: &mut Self::Domain) {}
}

impl<'tcx> Analysis<'tcx> for RWCountAnalysis<'tcx, '_, '_> {
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
