#![feature(let_chains)]
#![feature(exit_status_error)]

use std::{
    env,
    ffi::OsStr,
    path::{Path, PathBuf},
    process::{self, Command},
};

pub fn main() {
    match env::args().nth(1).as_ref().map(AsRef::as_ref) {
        Some("saft") => {
            recompile_with_mir();
        }
        Some("rustc") => {
            dispatch_rustc_call();
        }
        _ => unreachable!(),
    }
}

fn get_val_from_cli_flag(name: &str) -> Option<String> {
    let mut args = env::args();
    while let Some(arg) = args.next() {
        if arg.starts_with(name) {
            if arg.len() == name.len() {
                return args.next();
            } else if let Some(val) = arg.split('=').nth(1) {
                return Some(val.to_string());
            } else {
                return None;
            }
        }
    }
    None
}

fn get_manifest_metadata() -> cargo_metadata::Metadata {
    let manifest_path = get_val_from_cli_flag("--manifest-path")
        .map(|manifest_path| Path::new(&manifest_path).canonicalize().unwrap());

    let mut cmd = cargo_metadata::MetadataCommand::new();
    if let Some(manifest_path) = manifest_path {
        cmd.manifest_path(manifest_path);
    }

    cmd.exec().expect("Cannot query manifest metadata")
}

fn command_call_procedure(mut cmd: Command) {
    let exit_status = cmd
        .spawn()
        .expect("could not run cargo")
        .wait()
        .expect("failed to wait for cargo");

    if let Err(code) = exit_status.exit_ok() {
        process::exit(code.code().unwrap_or(-1));
    }
}

fn recompile_with_mir() {
    let metadata = get_manifest_metadata();
    let root = metadata.root_package().unwrap();

    // Pallets only have one target, and it is a lib
    let target = &root.targets[0];

    let mut cmd = Command::new("cargo");
    cmd.arg("check").arg("--lib");

    let cli_args = env::args().skip(2);

    // Add cargo flags
    for arg in cli_args {
        if arg.as_str() == "--" {
            // Every remaining args after -- are saft args
            break;
        }
        cmd.arg(arg);
    }

    // Force recompile to encode MIR for every dependencies
    cmd.env("RUSTFLAGS", "--cfg saft -Z always_encode_mir");

    // Use our `main` as a wrapper for rustc, where calls will be dispatched.
    // It will act as a recursive call to `main`, but control flow will go to `rustc` branch.
    cmd.env(
        "RUSTC_WRAPPER",
        env::current_exe().expect("Current executable path is invalid."),
    );

    // Set the target name for the recursive call for correct dispatch
    cmd.env("SAFT_TARGET", target.name.replace('-', "_"));

    command_call_procedure(cmd);
}

fn get_driver_path() -> PathBuf {
    let mut path = env::current_exe()
        .expect("Current executable path is invalid.")
        .with_file_name("saft-driver");

    if cfg!(windows) {
        path.set_extension("exe");
    }

    path
}

fn invoke_program_with_same_calling_args<S>(program: S)
where
    S: AsRef<OsStr>,
{
    let mut cmd = Command::new(program);

    // Copy all the cli args from calling procedure
    cmd.args(env::args().skip(2));

    command_call_procedure(cmd);
}

fn dispatch_rustc_call() {
    // If compiling the target crate,
    // give control to our driver for analysis.
    // Otherwise just compile.

    if let Some(crate_name) = get_val_from_cli_flag("--crate-name")
    && let Ok(target) = env::var("SAFT_TARGET")
    && target.eq(&crate_name)
    {
        invoke_program_with_same_calling_args(get_driver_path());
    } else {
        invoke_program_with_same_calling_args("rustc");
    }
}
