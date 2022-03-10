use crate::{analysis_utils, extrinsic_visitor::ExtrinsicVisitor};
use rustc_driver::Compilation;
use rustc_interface::{interface, Queries};
use rustc_middle::ty::TyCtxt;
use std::fmt::{Debug, Formatter, Result};

pub struct SaftCallbacks {}

impl SaftCallbacks {
    pub fn new() -> SaftCallbacks {
        SaftCallbacks {}
    }

    fn extract_juice<'tcx>(&mut self, _compiler: &interface::Compiler, tcx: TyCtxt<'tcx>) {
        // Retrieve the variants of the Call enum, aka names of extrinsics
        let variant_ids = analysis_utils::get_call_enum_variants_hir_ids(tcx);
        // Retrieve local def id of the 'dispatch_bypass_filter' function, aka the function that
        // dispatches the calls at the pallet level
        let dispatch_local_def_id = analysis_utils::get_dispatch_bypass_filter_local_def_id(tcx);

        let extrinsics_def_ids = if let Some(dispatch_local_def_id) = dispatch_local_def_id {
            analysis_utils::get_extrinsics_fn_ids(tcx, dispatch_local_def_id, &variant_ids)
        } else {
            println!("Pallet level dispatch function not found.\nFunction 'dispatch_bypass_filter' not found, are you running SAFT on the pallet level?");
            std::process::exit(1);
        };

        println!("The following extrinsics will be analyzed :");
        analysis_utils::print_extrinsics_names(tcx, Some(variant_ids));

        for extrinsics_def_id in extrinsics_def_ids {
            let mut extrinsic_visitor = ExtrinsicVisitor::new(tcx, extrinsics_def_id);
            println!("Analyzing {}...", extrinsic_visitor.get_fn_name());
            extrinsic_visitor.visit_body();
        }
    }
}

impl Debug for SaftCallbacks {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        "SaftCallbacks".fmt(f)
    }
}

impl Default for SaftCallbacks {
    fn default() -> Self {
        Self::new()
    }
}

impl rustc_driver::Callbacks for SaftCallbacks {
    fn config(&mut self, _config: &mut interface::Config) {}

    fn after_analysis<'tcx>(
        &mut self,
        compiler: &interface::Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        compiler.session().abort_if_errors();

        queries
            .global_ctxt()
            .unwrap()
            .peek_mut()
            .enter(|tcx| self.extract_juice(compiler, tcx));

        Compilation::Continue
    }
}
