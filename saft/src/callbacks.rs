use rustc_driver::Compilation;
use rustc_interface::{interface, Queries};
use std::{fmt::{Debug, Formatter, Result}, io::Write};
use rustc_middle::ty::TyCtxt;
use rustc_hir::def_id::{DefId, DefIndex};

pub struct SaftCallbacks {

}

impl SaftCallbacks {
    pub fn new() -> SaftCallbacks {
        SaftCallbacks {}
    }

    fn analyze<'tcx>(&mut self, compiler: &interface::Compiler, tcx: TyCtxt<'tcx>) {

    }

    fn print<'tcx>(&mut self, tcx: TyCtxt<'tcx>) {

        for local_def_id in tcx.hir().body_owners() {
            let def_id = local_def_id.to_def_id();
            
            let id = rustc_middle::ty::WithOptConstParam::unknown(def_id);
            let def = rustc_middle::ty::InstanceDef::Item(id);
            let mir = tcx.instance_mir(def);

            /*let mut stdout = std::io::stdout();
            stdout.write_fmt(format_args!("{:?}", def_id)).unwrap();
            rustc_middle::mir::write_mir_pretty(tcx, Some(def_id), &mut stdout).unwrap();
            let _ = stdout.flush();*/
        }

        for crate_num in tcx.crates(()) {
            println!("{}", tcx.crate_name(*crate_num));
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
    fn config(&mut self, _config: &mut interface::Config) {
        
    }

    fn after_analysis<'tcx>(
        &mut self, 
        compiler: &interface::Compiler, 
        queries: &'tcx Queries<'tcx>
    ) -> Compilation {
        compiler.session().abort_if_errors();

        queries.global_ctxt().unwrap().peek_mut().enter(|tcx| self.print(tcx));

        Compilation::Continue
    }
}
