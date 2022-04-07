use std::{
    env,
    path::PathBuf,
    process::{self, Command},
};

struct CargoSaftCmd {
    cargo_subcmd: &'static str,
    args: Vec<String>,
    saft_args: Vec<String>,
}

impl CargoSaftCmd {
    fn new<I>(mut old_args: I) -> Self
    where
        I: Iterator<Item = String>,
    {
        let cargo_subcmd = "check";
        let mut args = Vec::new();
        let mut saft_args = Vec::new();

        while let Some(arg) = old_args.next() {
            match arg.as_str() {
                "--single_dispatchable" => {
                    let dispatchable_name = old_args.next().expect("Missing dispatchable name");
                    saft_args.push(arg + "=" + &dispatchable_name);
                    continue;
                }
                "--" => break,
                _ => {}
            }
            args.push(arg);
        }

        // Every remaining arg is rustc arg
        args.append(&mut (old_args.collect()));

        Self {
            cargo_subcmd,
            args,
            saft_args,
        }
    }

    fn analyzer_path() -> PathBuf {
        let mut path = env::current_exe()
            .expect("invalid executable path")
            .with_file_name("saft-driver");

        if cfg!(windows) {
            path.set_extension("exe");
        }

        path
    }

    fn build_command(self) -> Command {
        let mut cmd = Command::new("cargo");
        let saft_args: String = self
            .saft_args
            .iter()
            .map(|opt| format!("{}{}", opt, saft_analysis::SAFT_OPT_SEPARATOR))
            .collect();

        cmd.env("RUSTC_WORKSPACE_WRAPPER", Self::analyzer_path())
            .env("SAFT_OPTIONS", saft_args)
            .arg(self.cargo_subcmd)
            .args(self.args);

        cmd
    }
}

fn process<I>(old_args: I) -> Result<(), i32>
where
    I: Iterator<Item = String>,
{
    let mut cmd = CargoSaftCmd::new(old_args).build_command();

    println!("{:?}", cmd);

    let exit_status = cmd
        .spawn()
        .expect("could not run cargo")
        .wait()
        .expect("failed to wait for cargo");

    if exit_status.success() {
        Ok(())
    } else {
        Err(exit_status.code().unwrap_or(-1))
    }
}

pub fn main() {
    if let Err(code) = process(env::args().skip(2)) {
        process::exit(code);
    }
}
