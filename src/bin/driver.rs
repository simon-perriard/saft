#![feature(rustc_private)]
#![feature(let_chains)]
#![warn(rustc::internal)]

extern crate rustc_driver;
extern crate rustc_interface;

use rustc_driver::Compilation;
use rustc_interface::{interface, Queries};
use std::{
    env,
    fmt::{Debug, Formatter, Result},
};

use log::info;

enum Action {
    Analyze,
    Compile,
}

use Action::{Analyze, Compile};

fn main() {
    use std::process::exit;

    // Initialize loggers.
    rustc_driver::init_rustc_env_logger();
    env_logger::init_from_env("SAFT_LOG");

    exit(rustc_driver::catch_with_exit_code(move || {
        // Prepare arguments.
        let mut args: Vec<String> = env::args().collect();
        args.remove(1); // remove the `cargo` subcommand
        saft::sysroot::set_if_missing(&mut args);

        // Analyze or compile.
        match select_action(&args) {
            Analyze => {
                info!("analyzing with args: {:?}", args);
                rustc_driver::RunCompiler::new(&args, &mut SaftCallbacks::new()).run()
            }
            Compile => {
                info!("compiling with args: {:?}", args);
                rustc_driver::RunCompiler::new(&args, &mut RustcCallbacks {}).run()
            }
        }
    }));
}

fn select_action(args: &Vec<String>) -> Action {
    if !should_emit_metadata(args) {
        return Compile;
    }
    if !is_cargo_primary_package(args) {
        return Compile;
    }
    return Analyze;
}

fn should_emit_metadata(args: &Vec<String>) -> bool {
    args.iter()
        .find(|arg| arg.starts_with("--emit=") && arg.contains("metadata"))
        .is_some()
}

fn is_cargo_primary_package(_args: &Vec<String>) -> bool {
    env::var("CARGO_PRIMARY_PACKAGE").is_ok()
}

#[derive(Default, Clone)]
struct RustcCallbacks {}

impl rustc_driver::Callbacks for RustcCallbacks {}

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
            .enter(saft::extract_juice);

        Compilation::Continue
    }
}
