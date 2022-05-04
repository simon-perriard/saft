use super::pallet::Pallet;
use super::r_w_count_domain::RWCountDomain;
use super::size_language::{HasSize, Size};
use super::storage_actions::HasAccessType;
use crate::rustc_mir_dataflow::JoinSemiLattice;
use crate::storage_actions::AccessType;
use rpds::HashTrieMap;
use rustc_middle::mir::{
    traversal::*, visit::*, BasicBlock, Body, Location, Operand, Statement, Terminator,
    TerminatorKind,
};
use rustc_middle::ty::{subst::SubstsRef, TyCtxt, TyKind};
use rustc_mir_dataflow::{Analysis, AnalysisDomain, CallReturnPlaces, Forward};
use rustc_span::def_id::DefId;
use std::cell::RefCell;
use std::rc::Rc;

pub(crate) type Summary = HashTrieMap<DefId, Option<RWCountDomain>>;

pub(crate) struct RWCountAnalysis<'tcx, 'inter, 'intra> {
    tcx: TyCtxt<'tcx>,
    pallet: &'inter Pallet,
    body: &'intra Body<'tcx>,
    def_id: DefId,
    pub summaries: Rc<RefCell<Summary>>,
    pub is_success: Rc<RefCell<bool>>,
}

impl<'tcx, 'inter, 'intra> RWCountAnalysis<'tcx, 'inter, 'intra> {
    pub(crate) fn new(tcx: TyCtxt<'tcx>, pallet: &'inter Pallet, def_id:DefId, body: &'intra Body<'tcx>) -> Self {
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
        state: &'intra mut RWCountDomain,
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
    _body: &'intra Body<'tcx>,
    state: &'intra mut RWCountDomain,
    is_success: Rc<RefCell<bool>>,
}

impl<'tcx, 'inter, 'intra> TransferFunction<'tcx, 'inter, 'intra> {
    pub fn new(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        summaries: Rc<RefCell<Summary>>,
        _def_id: DefId,
        _body: &'intra Body<'tcx>,
        state: &'intra mut RWCountDomain,
        is_success: Rc<RefCell<bool>>,
    ) -> Self {
        TransferFunction {
            tcx,
            pallet,
            summaries,
            _def_id,
            _body,
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

    fn t_visit_fn_call(&mut self, target_def_id: DefId, substs: &'tcx SubstsRef, args: Vec<Operand<'tcx>>) {
        if let (Some(access_type), size) = self.is_storage_call(target_def_id, substs) {
            if let TyKind::Closure(closure_def_id, _) = substs.last().unwrap().expect_ty().kind() {
                self.t_fn_call_analysis(*closure_def_id, args);
            }

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

    fn t_fn_call_analysis(&mut self, target_def_id: DefId, _args: Vec<Operand<'tcx>>) {
        if self.summaries.borrow_mut().contains_key(&target_def_id) {
            // We already have the summary for this function, retrieve it and return
            let summary = self.summaries.borrow_mut();
            let summary = summary.get(&target_def_id).unwrap();

            if let Some(summary) = summary {
                self.state.join(summary);
            } else {
                // we are in a recursive call, just ignore it
                println!("Recursive calls not supported.");
                *self.is_success.borrow_mut() = false;
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

            let mut results = RWCountAnalysis::new_with_init(
                self.tcx,
                self.pallet,
                target_def_id,
                target_mir,
                self.summaries.clone(),
                self.is_success.clone(),
            )
            .into_engine(self.tcx, target_mir)
            .pass_name("r_w_count_analysis")
            .iterate_to_fixpoint()
            .into_results_cursor(target_mir);

            let fn_call_success_anaylsis = *results.analysis().is_success.borrow();
            *self.is_success.borrow_mut() = fn_call_success_anaylsis;

            let end_state = if let Some((last, _)) = reverse_postorder(target_mir).last() {
                results.seek_to_block_end(last);
                Some(results.get().clone())
            } else {
                None
            };

            if let Some(end_state) = end_state {
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
            } else if path.starts_with("frame_system") && path.ends_with("deposit_event") {
                //println!("{:?}", args[0].ty(self.body, self.tcx).kind());
                //println!("{:?}", args[0].ty(self.body, self.tcx).is_sized(self.tcx.at(self.body.span), self.tcx.param_env(self.def_id)));
                //println!("{:?}", Type::from_mir_ty(self.tcx, args[0].ty(self.body, self.tcx)));
                
                // We need to track type of the event on the previous statements, it gets casted around

                /*
                    Deposit event will have it own specification
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
    fn visit_terminator(&mut self, terminator: &Terminator<'tcx>, location: Location) {
        let Terminator { source_info, kind } = terminator;

        match kind {
            TerminatorKind::Call {
                func: Operand::Constant(c),
                args,
                ..
            } if let TyKind::FnDef(target_def_id, substs) = c.ty().kind() => {
                self.visit_source_info(source_info);

                self.t_visit_fn_call(*target_def_id, substs, (*args).clone());
            }
            _ => self.super_terminator(terminator, location),
        }
    }
}

impl<'inter> AnalysisDomain<'inter> for RWCountAnalysis<'_, '_, '_> {
    type Domain = RWCountDomain;
    const NAME: &'static str = "ReadsWritesCountAnalysis";

    type Direction = Forward;

    fn bottom_value(&self, _body: &Body<'inter>) -> Self::Domain {
        RWCountDomain::new()
    }

    fn initialize_start_block(&self, _body: &Body<'inter>, _state: &mut Self::Domain) {
        // Function args do not affect the analysis
    }
}

impl<'tcx> Analysis<'tcx> for RWCountAnalysis<'tcx, '_, '_> {
    fn apply_statement_effect(
        &self,
        _state: &mut Self::Domain,
        _statement: &Statement<'tcx>,
        _location: Location,
    ) {
        // do nothing
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
