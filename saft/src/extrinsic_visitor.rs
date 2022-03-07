use rustc_middle::mir::Body;
use rustc_hir::def_id::DefId;
use rustc_middle::ty::TyCtxt;
use std::io::Write;

use crate::{analysis_utils, mir_visitor::MirVisitor};

pub struct ExtrinsicVisitor<'tcx, 'analysis> {
    pub name: String,
    pub full_path: String,
    pub def_id: DefId,
    pub tcx: TyCtxt<'tcx>,
    pub mir: &'analysis Body<'tcx>
}

impl<'tcx, 'analysis> ExtrinsicVisitor<'tcx, 'analysis> {
    pub fn new(tcx: TyCtxt<'tcx>, def_id: DefId,) -> Self {

        let id = rustc_middle::ty::WithOptConstParam::unknown(def_id);
        let def = rustc_middle::ty::InstanceDef::Item(id);
        let mir = tcx.instance_mir(def);

        ExtrinsicVisitor {
            name: analysis_utils::get_fn_name(tcx, def_id),
            full_path: analysis_utils::get_fn_name_with_path(tcx, def_id),
            def_id,
            tcx,
            mir
        }
    }

    pub fn print_mir(&self) {
        let mut stdout = std::io::stdout();
        stdout.write_fmt(format_args!("{:?}", self.def_id)).unwrap();
        rustc_middle::mir::write_mir_pretty(self.tcx, Some(self.def_id), &mut stdout).unwrap();
        let _ = stdout.flush();
    }

    pub fn get_fn_name(&self) -> String {
        self.name.clone()
    }

    pub fn get_fn_name_with_path(&self) -> String {
        self.full_path.clone()
    }

    pub fn visit_body(&mut self) {
        // At this point we have a rustc_middle::mir::Body
        let mut mir_visitor = MirVisitor::new(self);
        mir_visitor.start_visit();
    }
}