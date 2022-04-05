use rustc_hir::def_id::DefId;
use rustc_middle::mir::Body;
use rustc_middle::ty::TyCtxt;
use std::io::Write;

use crate::{analysis_utils::def_id_printer::*, mir_visitor::MirVisitor, typesystem_common::TySys};

pub struct ExtrinsicVisitor<'tcx, 'ts, 'extrinsic> {
    pub name: String,
    pub full_path: String,
    pub def_id: DefId,
    pub tcx: TyCtxt<'tcx>,
    pub mir: &'extrinsic Body<'tcx>,
    pub ts: &'ts TySys<'ts>,
}

impl<'tcx, 'ts, 'extrinsic> ExtrinsicVisitor<'tcx, 'ts, 'extrinsic> {
    pub fn new(tcx: TyCtxt<'tcx>, ts: &'ts TySys<'ts>, def_id: DefId) -> Self {
        
        let mir = tcx.optimized_mir(def_id);

        ExtrinsicVisitor {
            name: get_def_id_name(tcx, def_id),
            full_path: get_def_id_name_with_path(tcx, def_id),
            def_id,
            tcx,
            mir,
            ts,
        }
    }

    pub fn print_mir(&self) {
        let mut stdout = std::io::stdout();
        stdout.write_fmt(format_args!("{:?}", self.def_id)).unwrap();
        rustc_middle::mir::write_mir_pretty(self.tcx, Some(self.def_id), &mut stdout).unwrap();
        let _ = stdout.flush();
    }

    pub fn get_cloned_fn_name(&self) -> String {
        self.name.clone()
    }

    pub fn get_cloned_fn_name_with_path(&self) -> String {
        self.full_path.clone()
    }

    pub fn visit_body(&mut self) {
        // At this point we have a rustc_middle::mir::Body
        let mut mir_visitor = MirVisitor::new(self);
        mir_visitor.start_visit();

        let weights = mir_visitor.bodies_weights.get(&self.def_id).unwrap();
        println!("{}", weights);
    }
}
