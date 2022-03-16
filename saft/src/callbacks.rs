use crate::{analysis_utils::*, extrinsic_visitor::ExtrinsicVisitor};
use options::options::Options;
use rustc_driver::Compilation;
use rustc_interface::{interface, Queries};
use rustc_middle::ty::TyCtxt;
use std::fmt::{Debug, Formatter, Result};

pub struct SaftCallbacks {
    pub options: Options,
}

impl SaftCallbacks {
    pub fn new(options: Options) -> SaftCallbacks {
        SaftCallbacks { options }
    }

    fn extract_juice<'tcx>(&mut self, _compiler: &interface::Compiler, tcx: TyCtxt<'tcx>) {
        // Retrieve the variants of the Call enum, aka names of extrinsics
        let variant_ids = get_call_enum_variants_hir_ids(tcx);
        // Retrieve local def id of the 'dispatch_bypass_filter' function, aka the function that
        // dispatches the calls at the pallet level
        let dispatch_local_def_id = get_dispatch_bypass_filter_local_def_id(tcx);

        let extrinsics_def_ids = if let Some(dispatch_local_def_id) = dispatch_local_def_id {
            get_extrinsics_fn_ids(tcx, dispatch_local_def_id, &variant_ids)
        } else {
            println!("Pallet level dispatch function not found.\nFunction 'dispatch_bypass_filter' not found, are you running SAFT on the pallet level?");
            std::process::exit(1);
        };

        if let Some(single_function) = &self.options.single_func {
            println!("The following extrinsics will be analyzed :");
            println!("{}", single_function);

            let mut target_extrinsic_def_id = None;

            for id in &extrinsics_def_ids {
                if get_def_id_name(tcx, *id) == single_function.trim() {
                    target_extrinsic_def_id = Some(id);
                    break;
                }
            }

            if let Some(target_extrinsic_def_id) = target_extrinsic_def_id {
                let mut extrinsic_visitor = ExtrinsicVisitor::new(tcx, *target_extrinsic_def_id);
                println!("Analyzing {}...", extrinsic_visitor.get_fn_name());
                extrinsic_visitor.visit_body();
            } else {
                println!("Function {} not found.", single_function);
                std::process::exit(1);
            }
        } else {
            println!("The following extrinsics will be analyzed :");
            print_extrinsics_names(tcx, Some(variant_ids));

            for extrinsics_def_id in extrinsics_def_ids {
                let mut extrinsic_visitor = ExtrinsicVisitor::new(tcx, extrinsics_def_id);
                println!("Analyzing {}...", extrinsic_visitor.get_fn_name());
                extrinsic_visitor.visit_body();
            }
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
        Self::new(Options::default())
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

        queries.global_ctxt().unwrap().peek_mut().enter(
            |tcx| get_storage_variables(&tcx), /*self.extract_juice(compiler, tcx)*/
        );

        Compilation::Continue
    }
}
