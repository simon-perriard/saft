#![feature(rustc_private)]
#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(drain_filter)]
#![feature(if_let_guard)]

extern crate rustc_ast;
extern crate rustc_expand;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_dataflow;
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

    //println!("The following dispatchables will be analyzed :");
    //analysis_utils::dispatchables_getter::print_dispatchable_names(tcx, &pallet.dispatchables);

    for dispatchable_def_id in pallet.dispatchables.keys() {
        /*let mut dispatchable_visitor =
            dispatchable_visitor::DispatchableVisitor::new(tcx, &pallet, *dispatchables_def_id);
        print!("Analyzing {}...", dispatchable_visitor.get_fn_name());
        dispatchable_visitor.visit_body();
        println!(" Done")*/

        let storage_calls_analysis =
        storage_calls_analysis::StorageCallsAnalysis::new(tcx, &pallet);

        let mir = tcx.optimized_mir(dispatchable_def_id);
        let mut results = storage_calls_analysis
            .into_engine_with_def_id(tcx, mir, *dispatchable_def_id)
            .pass_name("storage_calls_analysis")
            .iterate_to_fixpoint()
            .into_results_cursor(mir);

        let state = if let Some(last) = mir.basic_blocks().last() {
            results.seek_to_block_end(last);
            Some(results.get().clone())
        } else { None };

        println!("{} --- {:?}", tcx.def_path_str(*dispatchable_def_id), state.unwrap().storage_accesses());
        println!();
    }
}
