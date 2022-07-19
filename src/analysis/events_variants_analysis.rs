use super::events_variants_domain::EventVariantsDomain;
use regex::Regex;
use rustc_middle::mir::{
    visit::*, BasicBlock, Body, Local, Location, Operand, Rvalue, Statement, StatementKind,
    Terminator, TerminatorKind,
};
use rustc_middle::ty::{TyCtxt, TyKind};
use rustc_mir_dataflow::{Analysis, AnalysisDomain, CallReturnPlaces, Forward};
use rustc_target::abi::VariantIdx;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub(crate) struct EventsVariantsAnalysis<'tcx, 'intra> {
    tcx: TyCtxt<'tcx>,
    body: &'intra Body<'tcx>,
    set_discriminant: Rc<RefCell<HashMap<Local, VariantIdx>>>,
}

impl<'tcx, 'intra> EventsVariantsAnalysis<'tcx, 'intra> {
    pub(crate) fn new(tcx: TyCtxt<'tcx>, body: &'intra Body<'tcx>) -> Self {
        Self::new_with_init(tcx, body, Rc::new(RefCell::new(HashMap::new())))
    }

    fn new_with_init(
        tcx: TyCtxt<'tcx>,
        body: &'intra Body<'tcx>,
        set_discriminant: Rc<RefCell<HashMap<Local, VariantIdx>>>,
    ) -> Self {
        EventsVariantsAnalysis {
            tcx,
            body,
            set_discriminant,
        }
    }

    pub(crate) fn transfer_function(
        &self,
        state: &'intra mut EventVariantsDomain,
    ) -> TransferFunction<'tcx, 'intra> {
        TransferFunction::new(self.tcx, self.body, self.set_discriminant.clone(), state)
    }
}

pub(crate) struct TransferFunction<'tcx, 'intra> {
    tcx: TyCtxt<'tcx>,
    body: &'intra Body<'tcx>,
    set_discriminant: Rc<RefCell<HashMap<Local, VariantIdx>>>,
    state: &'intra mut EventVariantsDomain,
}

impl<'tcx, 'intra> TransferFunction<'tcx, 'intra> {
    pub fn new(
        tcx: TyCtxt<'tcx>,
        body: &'intra Body<'tcx>,
        set_discriminant: Rc<RefCell<HashMap<Local, VariantIdx>>>,
        state: &'intra mut EventVariantsDomain,
    ) -> Self {
        TransferFunction {
            tcx,
            body,
            set_discriminant,
            state,
        }
    }
}

impl<'tcx> Visitor<'tcx> for TransferFunction<'tcx, '_> {
    fn visit_statement(&mut self, statement: &Statement<'tcx>, location: Location) {
        let Statement { source_info, kind } = statement;

        let event_regex = Regex::new(r"^pallet::Event<.*\s*(,.*)*>$").unwrap();

        match kind {
            StatementKind::SetDiscriminant {
                place,
                variant_index,
            } => {
                self.visit_source_info(source_info);

                //if event_regex.is_match(format!("{}",self.body.local_decls[place.local].ty).as_str()) {
                    // Set the discriminant for the local, we need it to track
                    // the Event enum variant that will be deposited
                    self.set_discriminant
                    .borrow_mut()
                    .insert(place.local, *variant_index);
                //}
            }
            /*StatementKind::Assign(box (place_to, r_value)) if let Rvalue::Use(operand) = r_value => {

                match operand {
                    Operand::Copy(place_from)
                    | Operand::Move(place_from) => {

                        let set_discriminant_map_copy = self.set_discriminant.borrow().clone();

                        if let Some(variant_index) = set_discriminant_map_copy.get(&place_from.local) && event_regex.is_match(format!("{}",self.body.local_decls[place_to.local].ty).as_str()) {
                            assert!(place_to.projection.is_empty());

                            self.set_discriminant
                            .borrow_mut()
                            .insert(place_to.local, *variant_index);
                        }
                    }
                    _ => (),
                }
            }*/
            _ => self.super_statement(statement, location),
        }
        //TODO: add support for StatementKind::Assign Use
    }

    fn visit_terminator(&mut self, terminator: &Terminator<'tcx>, location: Location) {
        let Terminator { kind, .. } = terminator;

        match kind {
            TerminatorKind::Call {
                func: Operand::Constant(c),
                args,
                ..
            } if let TyKind::FnDef(target_def_id, substs) = c.ty().kind() => {
                // Filtering event deposit, pallet::deposit_event will later call frame_system::deposit_event,
                // but we can only catch the variant of the Event enum now. Or we need to pass a calling context for
                // further analysis.
                let path = self.tcx.def_path_str(*target_def_id);
                if path.starts_with("pallet::Pallet") && path.ends_with("deposit_event") {
                    if let TyKind::Adt(_, _) = args[0].ty(self.body, self.tcx).kind() {
                        let variant_id = *self
                                .set_discriminant.borrow()
                                .get(&args[0].place().unwrap().local)
                                .unwrap();

                        self.state.add_variant(location, variant_id)
                    }
                }
            }
            _ => self.super_terminator(terminator, location),
        }
    }
}

impl<'intra> AnalysisDomain<'intra> for EventsVariantsAnalysis<'_, '_> {
    type Domain = EventVariantsDomain;
    const NAME: &'static str = "EventVariantsAnalysis";

    type Direction = Forward;

    fn bottom_value(&self, _body: &Body<'intra>) -> Self::Domain {
        EventVariantsDomain::new()
    }

    fn initialize_start_block(&self, _body: &Body<'intra>, _state: &mut Self::Domain) {}
}

impl<'tcx> Analysis<'tcx> for EventsVariantsAnalysis<'tcx, '_> {
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
