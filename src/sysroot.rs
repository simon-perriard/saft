use core::ops::Deref;
use log::{error, info};
use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;

pub fn get() -> Option<String> {
    None.or_else(from_env)
        .or_else(from_rustup)
        .or_else(from_multirust)
        .or_else(from_rustc)
        .or_else(from_compiletime_env)
        .or_else(from_compiletime_rustup)
        .or_else(from_compiletime_multirust)
        .map(|pb| pb.to_string_lossy().to_string())
}

pub fn set_if_missing(args: &mut Vec<String>) {
    if !args
        .iter()
        .any(|arg| arg == "--sysroot" || arg.starts_with("--sysroot="))
    {
        match get() {
            Some(sysroot) => {
                info!("sysroot={}", sysroot);
                if !Path::new(&sysroot).exists() {
                    error!("sysroot does not exist");
                }
                args.push("--sysroot".to_string());
                args.push(sysroot);
            }
            None => {
                error!("sysroot not specified");
            }
        }
    }
}

// Sysroot providers

fn from_rustup() -> Option<PathBuf> {
    let a = env::var("RUSTUP_HOME").ok()?;
    let b = env::var("RUSTUP_TOOLCHAIN").ok()?;
    Some(toolchain_path(a, b))
}

fn from_compiletime_rustup() -> Option<PathBuf> {
    let a = option_env!("RUSTUP_HOME")?;
    let b = option_env!("RUSTUP_TOOLCHAIN")?;
    Some(toolchain_path(a, b))
}

fn from_multirust() -> Option<PathBuf> {
    let a = env::var("MULTIRUST_HOME").ok()?;
    let b = env::var("MULTIRUST_TOOLCHAIN").ok()?;
    Some(toolchain_path(a, b))
}

fn from_compiletime_multirust() -> Option<PathBuf> {
    let a = option_env!("MULTIRUST_HOME")?;
    let b = option_env!("MULTIRUST_TOOLCHAIN")?;
    Some(toolchain_path(a, b))
}

fn toolchain_path<T: Deref<Target = str>>(home: T, toolchain: T) -> PathBuf {
    [home.deref(), "toolchains", toolchain.deref()]
        .iter()
        .collect()
}

fn from_env() -> Option<PathBuf> {
    env::var("SYSROOT").ok().map(PathBuf::from)
}

fn from_compiletime_env() -> Option<PathBuf> {
    option_env!("SYSROOT").map(PathBuf::from)
}

fn from_rustc() -> Option<PathBuf> {
    Command::new("rustc")
        .arg("--print")
        .arg("sysroot")
        .output()
        .ok()
        .and_then(|out| String::from_utf8(out.stdout).ok())
        .map(|s| PathBuf::from(s.trim()))
}
