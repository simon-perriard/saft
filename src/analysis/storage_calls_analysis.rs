use super::pallet::Pallet;
use super::storage_calls_domain::{StorageCallsDomain, AccessType};
use crate::pallet::Field;
use rpds::HashTrieMap;
use rustc_middle::mir::{
    visit::*, BasicBlock, Body, Location, Operand, Statement, Terminator, TerminatorKind,
};
use rustc_middle::ty::{subst::SubstsRef, TyCtxt, TyKind};
use rustc_mir_dataflow::{Analysis, AnalysisDomain, CallReturnPlaces, Forward};
use rustc_span::def_id::DefId;
use std::cell::RefCell;
use std::rc::Rc;
use crate::rustc_mir_dataflow::JoinSemiLattice;

pub(crate) type Summary = HashTrieMap<DefId, StorageCallsDomain>;

pub(crate) struct StorageCallsAnalysis<'tcx, 'inter> {
    tcx: TyCtxt<'tcx>,
    pallet: &'inter Pallet,
    summaries: Rc<RefCell<Summary>>,
}

impl<'tcx, 'inter, 'intra> StorageCallsAnalysis<'tcx, 'inter> {
    pub(crate) fn new(tcx: TyCtxt<'tcx>, pallet: &'inter Pallet) -> Self {
        Self::new_with_init(tcx, pallet, Rc::new(RefCell::new(HashTrieMap::new())))
    }

    fn new_with_init(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        summaries: Rc<RefCell<Summary>>,
    ) -> Self {
        StorageCallsAnalysis {
            tcx,
            pallet,
            summaries,
        }
    }

    pub(crate) fn transfer_function(
        &self,
        state: &'intra mut StorageCallsDomain,
    ) -> TransferFunction<'tcx, 'inter, 'intra> {
        TransferFunction::new(self.tcx, self.pallet, self.summaries.clone(), state)
    }
}

pub(crate) struct TransferFunction<'tcx, 'inter, 'intra> {
    tcx: TyCtxt<'tcx>,
    pallet: &'inter Pallet,
    summaries: Rc<RefCell<Summary>>,
    state: &'intra mut StorageCallsDomain,
}

impl<'tcx, 'inter, 'intra> TransferFunction<'tcx, 'inter, 'intra> {
    pub fn new(
        tcx: TyCtxt<'tcx>,
        pallet: &'inter Pallet,
        summaries: Rc<RefCell<Summary>>,
        state: &'intra mut StorageCallsDomain,
    ) -> Self {
        TransferFunction {
            tcx,
            pallet,
            summaries,
            state,
        }
    }
}

impl<'visitor, 'tcx> TransferFunction<'tcx, '_, '_>
where
    Self: Visitor<'visitor>,
{
    fn is_storage_call(&self, def_id: DefId, substs: &'tcx SubstsRef) -> Option<Field> {
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
                        return Some(field.clone());
                    }
                }
            }
        }
        None
    }

    fn t_visit_fn_call(
        &mut self,
        target_def_id: DefId,
        substs: &'tcx SubstsRef,
        location: Location,
    ) {
        if let Some(field) = self.is_storage_call(target_def_id, substs) {
            if let TyKind::Closure(closure_def_id, _) = substs.last().unwrap().expect_ty().kind() {
                self.t_fn_call_analysis(*closure_def_id, location);
            }

            self.state.add(location.block, AccessType::Direct(target_def_id, field));
        } else {
            self.t_fn_call_analysis(target_def_id, location);
        }
    }

    fn t_fn_call_analysis(&mut self, target_def_id: DefId, location: Location) {
        if self.summaries.borrow_mut().contains_key(&target_def_id) {
            // We already have the summary for this function
            //println!("ALREADY HAVE {:?} --- {:?}", self.tcx.def_path_str(target_def_id), self.summaries.borrow_mut().get(&target_def_id).unwrap());
            let summary = self.summaries.borrow_mut();
            let summary = summary.get(&target_def_id).unwrap();
            self.state.join(summary);
            return;
        }

        if self.tcx.is_mir_available(target_def_id) {
            let target_mir = self.tcx.optimized_mir(target_def_id);

            let mut results =
                StorageCallsAnalysis::new_with_init(self.tcx, self.pallet, self.summaries.clone())
                    .into_engine(self.tcx, target_mir)
                    .pass_name("storage_calls_analysis")
                    .iterate_to_fixpoint()
                    .into_results_cursor(target_mir);

            let end_state = if let Some(last) = target_mir.basic_blocks().last() {
                results.seek_to_block_end(last);
                Some(results.get().clone())
            } else {
                None
            };

            if let Some(end_state) = end_state {
                self.summaries
                    .borrow_mut()
                    .insert_mut(target_def_id, end_state.clone());

                // Mark domain that called function at that Location does storage access
                if !end_state.is_empty() {
                    self.state.add(location.block, AccessType::Indirect(target_def_id));
                }
            }
        } else {
            let path = self.tcx.def_path_str(target_def_id);
            if path.starts_with("std")
                || path.starts_with("alloc")
                || path.starts_with("frame_system::ensure_signed")
                || path.starts_with("weights")
                || path.starts_with("frame_system") && path.ends_with("deposit_event")
            {
                /* No MIR available for those but:
                    - standard library does not do storage access
                    - ensure_signed does not do storage access
                    - weights do not do storage access
                    - deposit_event does not do storage access
                **/
            } else {
                println!(
                    "NO MIR AVAILABLE FOR {:?}",
                    self.tcx.def_path_str(target_def_id)
                );
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
                for arg in args {
                    self.visit_operand(arg, location);
                }

                self.t_visit_fn_call(*target_def_id, substs, location);
            }
            _ => self.super_terminator(terminator, location),
        }
    }
}

impl<'inter> AnalysisDomain<'inter> for StorageCallsAnalysis<'_, '_> {
    type Domain = StorageCallsDomain;
    const NAME: &'static str = "StorageCallsAnalysis";

    type Direction = Forward;

    fn bottom_value(&self, _body: &Body<'inter>) -> Self::Domain {
        StorageCallsDomain::new()
    }

    fn initialize_start_block(&self, _body: &Body<'inter>, _state: &mut Self::Domain) {
        // Function args do not affect the analysis
    }
}

impl<'tcx> Analysis<'tcx> for StorageCallsAnalysis<'tcx, '_> {
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
