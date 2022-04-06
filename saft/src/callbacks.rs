use crate::{
    analysis_utils::def_id_printer::*, dispatchable_visitor::DispatchableVisitor, pallet::Pallet,
};
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
        let pallet = Pallet::new(tcx);

        if let Some(single_function) = &self.options.single_func {
            println!("The following dispatchables will be analyzed :");
            println!("{}", single_function);

            let mut target_dispatchable_def_id = None;

            for id in pallet.dispatchables.keys() {
                if get_def_id_name(tcx, *id) == single_function.trim() {
                    target_dispatchable_def_id = Some(id);
                    break;
                }
            }

            if let Some(target_dispatchable_def_id) = target_dispatchable_def_id {
                let mut dispatchable_visitor =
                    DispatchableVisitor::new(tcx, &pallet, *target_dispatchable_def_id);
                println!("Analyzing {}...", dispatchable_visitor.get_fn_name());
                dispatchable_visitor.visit_body();
            } else {
                println!("Function {} not found.", single_function);
                std::process::exit(1);
            }
        } else {
            /*println!("The following dispatchables will be analyzed :");
            print_dispatchables_names(tcx, pallet.functions);*/

            for dispatchables_def_id in pallet.dispatchables.keys() {
                let mut dispatchable_visitor = DispatchableVisitor::new(tcx, &pallet, *dispatchables_def_id);
                println!("Analyzing {}...", dispatchable_visitor.get_fn_name());
                dispatchable_visitor.visit_body();
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

        queries
            .global_ctxt()
            .unwrap()
            .peek_mut()
            .enter(|tcx| self.extract_juice(compiler, tcx));

        Compilation::Continue
    }
}
