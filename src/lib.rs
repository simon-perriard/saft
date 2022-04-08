#![feature(rustc_private)]
#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(drain_filter)]

extern crate rustc_ast;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_span;
extern crate rustc_typeck;

pub mod analysis;
pub mod sysroot;

use crate::analysis::*;

pub fn extract_juice<'tcx>(tcx: rustc_middle::ty::TyCtxt<'tcx>) {
    // Extract pallet
    print!("Extracting pallet information...");
    let pallet = pallet::Pallet::new(tcx);
    println!(" Done");

    println!("The following dispatchables will be analyzed :");
    analysis_utils::dispatchables_getter::print_dispatchable_names(tcx, &pallet.dispatchables);

    for dispatchables_def_id in pallet.dispatchables.keys() {
        let mut dispatchable_visitor =
            dispatchable_visitor::DispatchableVisitor::new(tcx, &pallet, *dispatchables_def_id);
        print!("Analyzing {}...", dispatchable_visitor.get_fn_name());
        dispatchable_visitor.visit_body();
        println!(" Done")
    }
}
