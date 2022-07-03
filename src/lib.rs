#![feature(rustc_private)]
#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(drain_filter)]
#![feature(if_let_guard)]
#![feature(let_else)]
#![feature(is_some_with)]

extern crate rustc_ast;
extern crate rustc_expand;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_dataflow;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_typeck;

pub mod analysis;
pub mod analysis_drivers;
pub mod sysroot;
use crate::analysis::{analysis_utils, pallet};

pub fn extract_juice(tcx: rustc_middle::ty::TyCtxt) {
    // Extract pallet information
    print!("Extracting pallet information...");
    let pallet = pallet::Pallet::new(tcx);
    println!(" Done");

    // Extract event variants at callsite
    print!("Extracting event variants...");
    let events_variants =
        analysis_drivers::events_variants_analysis_driver::events_variants_analysis(tcx);
    println!(" Done");

    println!("The following dispatchables will be analyzed :");
    analysis_utils::dispatchables_getter::print_dispatchable_names(tcx, &pallet.dispatchables);
    println!();

    analysis_drivers::cost_analysis_driver::cost_analysis(tcx, pallet, events_variants);
}
