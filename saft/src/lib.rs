#![feature(rustc_private)]
#![feature(box_patterns)]

extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_hir;
extern crate rustc_session;
extern crate rustc_index;
extern crate rustc_span;

pub mod callbacks;
pub mod analysis_utils;
pub mod extrinsic_visitor;
pub mod mir_visitor;