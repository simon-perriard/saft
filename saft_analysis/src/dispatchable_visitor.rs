use rustc_hir::def_id::DefId;
use rustc_middle::mir::Body;
use rustc_middle::ty::TyCtxt;
use std::io::Write;

use crate::{analysis_utils::def_id_printer::*, mir_visitor::MirVisitor, pallet::Pallet};

pub struct DispatchableVisitor<'tcx, 'pallet> {
    pub def_id: DefId,
    pub tcx: TyCtxt<'tcx>,
    pub mir: &'tcx Body<'tcx>,
    pub pallet: &'pallet Pallet,
}

impl<'tcx, 'pallet> DispatchableVisitor<'tcx, 'pallet> {
    pub fn new(tcx: TyCtxt<'tcx>, pallet: &'pallet Pallet, def_id: DefId) -> Self {
        let id = rustc_middle::ty::WithOptConstParam::unknown(def_id);
        let def = rustc_middle::ty::InstanceDef::Item(id);
        let mir = tcx.instance_mir(def);

        DispatchableVisitor {
            def_id,
            tcx,
            mir,
            pallet,
        }
    }

    pub fn print_mir(&self) {
        let mut stdout = std::io::stdout();
        stdout.write_fmt(format_args!("{:?}", self.def_id)).unwrap();
        rustc_middle::mir::write_mir_pretty(self.tcx, Some(self.def_id), &mut stdout).unwrap();
        let _ = stdout.flush();
    }

    pub fn get_fn_name(&self) -> String {
        get_def_id_name(self.tcx, self.def_id)
    }

    pub fn get_fn_name_with_path(&self) -> String {
        self.tcx.def_path_str(self.def_id)
    }

    pub fn visit_body(&mut self) {
        // At this point we have a rustc_middle::mir::Body
        let mut mir_visitor = MirVisitor::new(self);
        mir_visitor.start_visit();

        let weights = mir_visitor.bodies_weights.get(&self.def_id).unwrap();
        //println!("{}", weights);
    }
}
