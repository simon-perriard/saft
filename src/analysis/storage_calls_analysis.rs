use rustc_middle::mir::{
    visit::Visitor, BasicBlock, Body, Location, Operand, Statement, Terminator, TerminatorKind,
};
use rustc_middle::ty::{TyCtxt, TyKind, subst::SubstsRef};
use rustc_mir_dataflow::{
    Analysis, AnalysisDomain, CallReturnPlaces, Forward,
};
use rustc_span::def_id::DefId;
use super::pallet::Pallet;
use super::storage_calls_domain::StorageCallsDomain;

pub struct StorageCallsAnalysis<'tcx, 'inter> {
    tcx: TyCtxt<'tcx>,
    pallet: &'inter Pallet,
}

impl<'tcx, 'inter> StorageCallsAnalysis<'tcx, 'inter> {
    pub(crate) fn new(tcx: TyCtxt<'tcx>, pallet: &'inter Pallet) -> Self {
        StorageCallsAnalysis { tcx, pallet }
    }
}

pub struct TransferFunction<'tcx> {
    pub tcx: TyCtxt<'tcx>,
}

impl<'visitor> TransferFunction<'_>
where
    Self: Visitor<'visitor>,
{
    fn t_visit_fn_call(&mut self, def_id: DefId, substs: SubstsRef, location: Location) {
        
    }
}

impl<'inter> Visitor<'inter> for TransferFunction<'_> {
    fn visit_terminator(&mut self, terminator: &Terminator<'inter>, location: Location) {
        let Terminator { source_info, kind } = terminator;

        match kind {
            TerminatorKind::Call {
                func: Operand::Constant(c),
                ..
            } if let TyKind::FnDef(def_id, substs) =  c.ty().kind() => {
                self.visit_source_info(source_info);
                self.t_visit_fn_call(*def_id, substs, location);
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

    fn initialize_start_block(&self, _body: &Body<'inter>, _state: &mut Self::Domain) {}
}

impl<'tcx, 'inter, 'intra> Analysis<'intra> for StorageCallsAnalysis<'tcx, 'inter> {
    fn apply_statement_effect(
        &self,
        state: &mut Self::Domain,
        statement: &Statement<'intra>,
        location: Location,
    ) {
    }

    fn apply_terminator_effect(
        &self,
        state: &mut Self::Domain,
        terminator: &Terminator<'intra>,
        location: Location,
    ) {
    }

    fn apply_call_return_effect(
        &self,
        _state: &mut Self::Domain,
        _block: BasicBlock,
        _return_place: CallReturnPlaces<'_, 'intra>,
    ) {
        // do nothing
    }
}
