use std::env;
use std::process::Command;

// Unfortunately, we cannot run the analysis directly on a given crate of interest. The reason is
// that the given crate may depend on other crates, and they must be passed as arguments to `rustc`.
// However, `cargo` does not offer a simple way to obtain these arguments.  That is why, we employ a
// standard trick, running the analysis as part of a `cargo check`.  We instruct `cargo` to use our
// driver for checking crates in the workspace.  For each check, `cargo` invokes the driver with the
// required arguments.  Then, the driver runs `rustc` and hooks at convenient stages to perform the
// analysis.  After the analysis is finished, the driver lets `rustc` emit the metadata it computed
// about the current crate, because this metadata may be needed for checking other crates.
fn main() {
    // Obtain driver from the current executable.
    let driver_path = env::current_exe()
        .expect("invalid current executable path")
        .with_file_name("saft-driver");

    // Run the analysis as part of a `cargo check`.
    Command::new("cargo")
        .env("RUSTC_WORKSPACE_WRAPPER", driver_path) // analyze only crates in the workspace
        .env("RUSTCFLAGS", "-Z always-encode-mir") // emit mir for all dependencies
        .arg("check")
        .arg("--lib")
        .args(env::args().skip(2))
        .spawn()
        .expect("could not run cargo")
        .wait()
        .expect("failed to wait for cargo");
}
