use clap::{Arg, Command, ErrorKind};
use itertools::Itertools;
use rustc_session::config::ErrorOutputType;
use rustc_session::early_error;
extern crate clap;

#[derive(Debug, Default)]
pub struct Options {
    pub single_func: Option<String>,
}

impl Options {
    /// Parse options from an argument string. The argument string will be split using unix
    /// shell escaping rules. Any content beyond the leftmost `--` token will be returned
    /// (excluding this token).
    pub fn parse_from_str(&mut self, s: &str) -> Vec<String> {
        self.parse(&shellwords::split(s).unwrap_or_else(|e| {
            early_error(
                ErrorOutputType::default(),
                &format!("Cannot parse argument string: {:?}", e),
            )
        }))
    }

    /// Parses options from a list of strings. Any content beyond the leftmost `--` token
    /// will be returned (excluding this token).
    pub fn parse(&mut self, args: &[String]) -> Vec<String> {
        let mut saft_args_end = args.len();
        let mut rustc_args_start = 0;
        if let Some((p, _)) = args.iter().find_position(|s| s.as_str() == "--") {
            saft_args_end = p;
            rustc_args_start = p + 1;
        }
        let saft_args = &args[0..saft_args_end];
        let matches = if rustc_args_start == 0 {
            // The arguments may not be intended for SAFT and may get here
            // via some tool, so do not report errors here, but just assume
            // that the arguments were not meant for SAFT.
            match make_options_parser().try_get_matches_from(saft_args.iter()) {
                Ok(matches) => {
                    // Looks like these are SAFT options after all and there are no rustc options.
                    rustc_args_start = args.len();
                    matches
                }
                Err(err) => {
                    let clap::Error { .. } = err;
                    match err.kind() {
                        ErrorKind::DisplayHelp => {
                            // help is ambiguous, so display both SAFT and rustc help.
                            println!("{:?}\n", err.context().collect_vec());
                            return args.to_vec();
                        }
                        ErrorKind::UnknownArgument => {
                            // Just send all of the arguments to rustc.
                            // Note that this means that SAFT options and rustc options must always
                            // be separated by --. I.e. any  SAFT options present in arguments list
                            // will stay unknown to SAFT and will make rustc unhappy.
                            return args.to_vec();
                        }
                        _ => {
                            err.exit();
                        }
                    }
                }
            }
        } else {
            // This will display error diagnostics for arguments that are not valid for SAFT.
            make_options_parser().get_matches_from(saft_args.iter())
        };

        if matches.is_present("single_func") {
            self.single_func = matches.value_of("single_func").map(|s| s.to_string());
        }

        args[rustc_args_start..].to_vec()
    }
}

fn make_options_parser<'a>() -> Command<'a> {
    Command::new("SAFT")
        .no_binary_name(true)
        .version("v0.0.1")
        .arg(
            Arg::new("single_func")
                .long("single_func")
                .takes_value(true)
                .help("Focus analysis on the named function."),
        )
}