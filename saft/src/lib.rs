#![feature(rustc_private)]

extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_hir;
extern crate rustc_session;

pub mod callbacks;
pub mod analysis_utils;
pub mod extrinsic_visitor;