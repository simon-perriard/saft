#![feature(rustc_private)]

extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_session;
extern crate rustc_span;

use rustc_driver::Compilation;
use rustc_interface::{interface, Queries};
use std::{
    env,
    fmt::{Debug, Formatter, Result},
    path::Path,
    process::exit,
};

#[derive(Default, Clone)]
struct SaftCallbacks {}

impl SaftCallbacks {
    fn new() -> SaftCallbacks {
        SaftCallbacks {}
    }
}

impl Debug for SaftCallbacks {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        "SaftCallbacks".fmt(f)
    }
}

impl rustc_driver::Callbacks for SaftCallbacks {
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
            .enter(|tcx| saft_analysis::extract_juice(compiler, tcx));

        Compilation::Continue
    }
}

fn get_sysroot() -> String {
    let home = option_env!("RUSTUP_HOME");
    let toolchain = option_env!("RUSTUP_TOOLCHAIN");
    match (home, toolchain) {
        (Some(home), Some(toolchain)) => format!("{}/toolchains/{}", home, toolchain),
        _ => option_env!("RUST_SYSROOT")
            .expect("Could not find sysroot.")
            .to_owned(),
    }
}

fn main() {
    rustc_driver::init_rustc_env_logger();
    rustc_driver::install_ice_hook();
    exit(rustc_driver::catch_with_exit_code(move || {
        let mut args: Vec<String> = env::args().collect();

        // We will invoke the compiler programmatically, so we only need what's after the 'rustc'
        if args.len() > 1 && Path::new(&args[1]).file_stem() == Some("rustc".as_ref()) {
            args.remove(1);
        }

        let sysroot: String = "--sysroot".into();
        if !args.iter().any(|arg| arg.starts_with(&sysroot)) {
            args.push(sysroot);
            args.push(get_sysroot());
        }

        rustc_driver::RunCompiler::new(&args, &mut SaftCallbacks::new()).run()
    }))
}
