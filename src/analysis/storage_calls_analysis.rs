use super::pallet::Pallet;
use super::storage_calls_domain::StorageCallsDomain;
use rpds::HashTrieSet;
use rustc_middle::mir::{
    visit::*, BasicBlock, Body, Location, Operand, Statement, Terminator, TerminatorKind,
};
use rustc_middle::ty::{subst::SubstsRef, Ty, TyCtxt, TyKind};
use rustc_mir_dataflow::{Analysis, AnalysisDomain, CallReturnPlaces, Engine, Forward};
use rustc_span::def_id::DefId;

pub struct StorageCallsAnalysis<'tcx, 'intra> {
    tcx: TyCtxt<'tcx>,
    pallet: &'intra Pallet,
}

impl<'tcx, 'intra> StorageCallsAnalysis<'tcx, 'intra> {
    pub(crate) fn new(tcx: TyCtxt<'tcx>, pallet: &'intra Pallet) -> Self {
        Self::new_with_init(tcx, pallet)
    }

    fn new_with_init(
        tcx: TyCtxt<'tcx>,
        pallet: &'intra Pallet,
    ) -> Self {
        StorageCallsAnalysis {
            tcx,
            pallet,
        }
    }

    pub(crate) fn into_engine_with_def_id<'mir>(
        mut self,
        tcx: TyCtxt<'tcx>,
        body: &'mir Body<'tcx>,
        entry_def_id: DefId,
    ) -> Engine<'mir, 'tcx, Self>
    where
        Self: Sized,
    {
        Engine::new_generic(tcx, body, self)
    }

    pub(crate) fn transfer_function(
        &self,
        state: &'intra mut StorageCallsDomain,
    ) -> TransferFunction<'tcx, 'intra> {
        TransferFunction::new(self.tcx, self.pallet, state)
    }
}

pub(crate) struct TransferFunction<'tcx, 'intra> {
    tcx: TyCtxt<'tcx>,
    pallet: &'intra Pallet,
    state: &'intra mut StorageCallsDomain,
}

impl<'tcx, 'intra> TransferFunction<'tcx, 'intra> {
    pub fn new(
        tcx: TyCtxt<'tcx>,
        pallet: &'intra Pallet,
        state: &'intra mut StorageCallsDomain,
    ) -> Self {
        TransferFunction {
            tcx,
            pallet,
            state,
        }
    }
}

impl<'visitor, 'tcx> TransferFunction<'tcx, '_>
where
    Self: Visitor<'visitor>,
{
    fn is_storage_call(&self, def_id: DefId, substs: &'tcx SubstsRef) -> Option<Ty> {
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
                for ty in pallet
                    .fields
                    .keys()
                    .map(|field_def_id| tcx.type_of(field_def_id))
                {
                    if ty == reconstructed_ty {
                        return Some(ty);
                    }
                }
            }
        }
        None
    }

    fn t_visit_fn_call(&mut self, def_id: DefId, substs: &'tcx SubstsRef, location: Location) {
        if let Some(_ty) = self.is_storage_call(def_id, substs) {
            self.state.add(location.block);
        } else {
            // We do intra analysis, if we cannot resolve the function, let's assume it will do storage access
            self.state.add(location.block);
            // TODO perform inter analysis
        }
    }
}

impl<'intra, 'tcx> Visitor<'tcx> for TransferFunction<'tcx, '_> {
    fn visit_terminator(&mut self, terminator: &Terminator<'tcx>, location: Location) {
        let Terminator { source_info, kind } = terminator;

        match kind {
            TerminatorKind::Call {
                func: Operand::Constant(c),
                args,
                ..
            } => {
                for arg in args {
                    self.super_operand(arg, location);
                }

                self.visit_source_info(source_info);
                if let TyKind::FnDef(def_id, substs) = c.ty().kind() {
                    self.t_visit_fn_call(*def_id, substs, location);
                }
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

impl<'tcx, 'inter, 'intra> Analysis<'tcx> for StorageCallsAnalysis<'tcx, 'inter> {
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
