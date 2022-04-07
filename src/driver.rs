#![feature(rustc_private)]

extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_session;
extern crate rustc_span;

use cargo_metadata::{Package, Target};
use rustc_driver::Compilation;
use rustc_interface::{interface, Queries};
use rustc_session::parse::ParseSess;
use rustc_span::symbol::Symbol;
use rustc_tools_util::{get_version_info, VersionInfo};
use std::{
    env,
    fmt::{Debug, Formatter, Result},
    ops::Deref,
    path::{Path, PathBuf},
    process::{exit, Command}, ffi::OsString,
};

#[derive(Default, Clone)]
struct SaftCallbacks {
    saft_args_var: Option<String>,
}

impl SaftCallbacks {
    fn new(saft_args_var: Option<String>) -> SaftCallbacks {
        SaftCallbacks { saft_args_var }
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

        let parsed_options = saft_analysis::parse_options(self.saft_args_var.clone());

        queries
            .global_ctxt()
            .unwrap()
            .peek_mut()
            .enter(|tcx| saft_analysis::extract_juice(compiler, tcx, parsed_options));

        Compilation::Continue
    }
}

struct DefaultCallbacks;
impl rustc_driver::Callbacks for DefaultCallbacks {}

/// This is different from `DefaultCallbacks` that it will inform Cargo to track the value of
/// `SAFT_ARGS` environment variable.
struct RustcCallbacks {
    saft_args_var: Option<String>,
}

fn track_saft_args(parse_sess: &mut ParseSess, args_env_var: &Option<String>) {
    parse_sess.env_depinfo.get_mut().insert((
        Symbol::intern("SAFT_ARGS"),
        args_env_var.as_deref().map(Symbol::intern),
    ));
}

impl rustc_driver::Callbacks for RustcCallbacks {
    fn config(&mut self, config: &mut interface::Config) {
        let saft_args_var = self.saft_args_var.take();
        config.parse_sess_created = Some(Box::new(move |parse_sess| {
            track_saft_args(parse_sess, &saft_args_var);
        }));
    }
}

/// If a command-line option matches `find_arg`, then apply the predicate `pred` on its value. If
/// true, then return it. The parameter is assumed to be either `--arg=value` or `--arg value`.
fn arg_value<'a, T: Deref<Target = str>>(
    args: &'a [T],
    find_arg: &str,
    pred: impl Fn(&str) -> bool,
) -> Option<&'a str> {
    let mut args = args.iter().map(Deref::deref);
    while let Some(arg) = args.next() {
        let mut arg = arg.splitn(2, '=');
        if arg.next() != Some(find_arg) {
            continue;
        }

        match arg.next().or_else(|| args.next()) {
            Some(v) if pred(v) => return Some(v),
            _ => {}
        }
    }
    None
}

fn toolchain_path(home: Option<String>, toolchain: Option<String>) -> Option<PathBuf> {
    home.and_then(|home| {
        toolchain.map(|toolchain| {
            let mut path = PathBuf::from(home);
            path.push("toolchains");
            path.push(toolchain);
            path
        })
    })
}
  
pub fn main() {
    rustc_driver::init_rustc_env_logger();

    exit(rustc_driver::catch_with_exit_code(move || {
        let mut orig_args: Vec<String> = env::args().collect();

        // Get the sysroot, looking from most specific to this invocation to the least:
        // - command line
        // - runtime environment
        //    - SYSROOT
        //    - RUSTUP_HOME, MULTIRUST_HOME, RUSTUP_TOOLCHAIN, MULTIRUST_TOOLCHAIN
        // - sysroot from rustc in the path
        // - compile-time environment
        //    - SYSROOT
        //    - RUSTUP_HOME, MULTIRUST_HOME, RUSTUP_TOOLCHAIN, MULTIRUST_TOOLCHAIN
        let sys_root_arg = arg_value(&orig_args, "--sysroot", |_| true);
        let have_sys_root_arg = sys_root_arg.is_some();
        let sys_root = sys_root_arg
            .map(PathBuf::from)
            .or_else(|| std::env::var("SYSROOT").ok().map(PathBuf::from))
            .or_else(|| {
                let home = std::env::var("RUSTUP_HOME")
                    .or_else(|_| std::env::var("MULTIRUST_HOME"))
                    .ok();
                let toolchain = std::env::var("RUSTUP_TOOLCHAIN")
                    .or_else(|_| std::env::var("MULTIRUST_TOOLCHAIN"))
                    .ok();
                toolchain_path(home, toolchain)
            })
            .or_else(|| {
                Command::new("rustc")
                    .arg("--print")
                    .arg("sysroot")
                    .output()
                    .ok()
                    .and_then(|out| String::from_utf8(out.stdout).ok())
                    .map(|s| PathBuf::from(s.trim()))
            })
            .or_else(|| option_env!("SYSROOT").map(PathBuf::from))
            .or_else(|| {
                let home = option_env!("RUSTUP_HOME")
                    .or(option_env!("MULTIRUST_HOME"))
                    .map(ToString::to_string);
                let toolchain = option_env!("RUSTUP_TOOLCHAIN")
                    .or(option_env!("MULTIRUST_TOOLCHAIN"))
                    .map(ToString::to_string);
                toolchain_path(home, toolchain)
            })
            .map(|pb| pb.to_string_lossy().to_string())
            .expect("need to specify SYSROOT env var during saft compilation, or use rustup or multirust");

        // make "saft-driver --rustc" work like a subcommand that passes further args to "rustc"
        // for example `saft-driver --rustc --version` will print the rustc version that saft-driver
        // uses
        if let Some(pos) = orig_args.iter().position(|arg| arg == "--rustc") {
            orig_args.remove(pos);
            orig_args[0] = "rustc".to_string();

            // if we call "rustc", we need to pass --sysroot here as well
            let mut args: Vec<String> = orig_args.clone();
            if !have_sys_root_arg {
                args.extend(vec!["--sysroot".into(), sys_root]);
            };

            return rustc_driver::RunCompiler::new(&args, &mut DefaultCallbacks).run();
        }

        if orig_args.iter().any(|a| a == "--version" || a == "-V") {
            let version_info = get_version_info!();
            println!("{}", version_info);
            exit(0);
        }

        // Setting RUSTC_WRAPPER causes Cargo to pass 'rustc' as the first argument.
        // We're invoking the compiler programmatically, so we ignore this/
        let wrapper_mode =
            orig_args.get(1).map(Path::new).and_then(Path::file_stem) == Some("rustc".as_ref());

        if wrapper_mode {
            // we still want to be able to invoke it normally though
            orig_args.remove(1);
        }

        if !wrapper_mode
            && (orig_args.iter().any(|a| a == "--help" || a == "-h") || orig_args.len() == 1)
        {
            //display_help();
            exit(0);
        }

        // this conditional check for the --sysroot flag is there so users can call
        // `saft_driver` directly
        // without having to pass --sysroot or anything
        let mut args: Vec<String> = orig_args.clone();
        if !have_sys_root_arg {
            args.extend(vec!["--sysroot".into(), sys_root]);
        };

        let mut no_deps = false;
        let saft_args_var = env::var("SAFT_ARGS").ok();
        let saft_args = saft_args_var
            .as_deref()
            .unwrap_or_default()
            .split(saft_analysis::SAFT_OPT_SEPARATOR)
            .filter_map(|s| match s {
                "" => None,
                "--single_dispatchable" => {
                    no_deps = true;
                    None
                }
                _ => Some(s.to_string()),
            })
            .chain(vec!["--cfg".into(), r#"feature="cargo-saft""#.into()])
            .collect::<Vec<String>>();

        args.extend(vec!["-Z".into(), "always-encode-mir".into()]);
        args.extend(saft_args);

        rustc_driver::RunCompiler::new(&args, &mut SaftCallbacks::new(saft_args_var)).run()
    }))
}


