use rustc_driver::Compilation;
use rustc_interface::{interface, Queries};
use std::{fmt::{Debug, Formatter, Result}, io::Write};
use rustc_middle::ty::TyCtxt;
use rustc_hir::def_id::{DefId, DefIndex};
use crate::{analysis_utils, extrinsic_visitor::ExtrinsicVisitor};

pub struct SaftCallbacks {

}

impl SaftCallbacks {
    pub fn new() -> SaftCallbacks {
        SaftCallbacks {}
    }

    fn analyze<'tcx>(&mut self, compiler: &interface::Compiler, tcx: TyCtxt<'tcx>) {

        // analyze public functions from the pallet
        let variant_ids = analysis_utils::get_call_enum_variants_hir_ids(tcx);

        let dispatch_local_def_id = analysis_utils::get_dispatch_bypass_filter_local_def_id(tcx);

        if let Some(dispatch_local_def_id) = dispatch_local_def_id {
            //analysis_utils::get_extrinsics_fn_ids(tcx, dispatch_local_def_id, &variant_ids);
            let fn_local_def_ids = analysis_utils::get_extrinsics_ids_WEAK(tcx, &variant_ids);

            for id in fn_local_def_ids {
                let def_id = id.to_def_id();
                let mut stdout = std::io::stdout();
                stdout.write_fmt(format_args!("{:?}", def_id)).unwrap();
                rustc_middle::mir::write_mir_pretty(tcx, Some(def_id), &mut stdout).unwrap();
                let _ = stdout.flush();
            }

        } else {
            println!("Pallet level dispatch function not found.\nFunction 'dispatch_bypass_filter' not found, are you running SAFT on the pallet level?");
            std::process::exit(1);
        }

        println!("The following extrinsics will be analyzed :");
        analysis_utils::print_extrinsics_names(tcx, Some(variant_ids));
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
    fn config(&mut self, _config: &mut interface::Config) {
        
    }

    fn after_analysis<'tcx>(
        &mut self, 
        compiler: &interface::Compiler, 
        queries: &'tcx Queries<'tcx>
    ) -> Compilation {
        compiler.session().abort_if_errors();

        queries.global_ctxt().unwrap().peek_mut().enter(|tcx| self.analyze(compiler, tcx));

        Compilation::Continue
    }
}
