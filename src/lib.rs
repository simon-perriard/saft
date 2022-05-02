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
extern crate rustc_data_structures;

pub mod analysis;
pub mod sysroot;
use crate::analysis::*;
use rustc_mir_dataflow::Analysis;

pub fn extract_juice<'tcx>(tcx: rustc_middle::ty::TyCtxt<'tcx>) {
    // Extract pallet
    print!("Extracting pallet information...");
    let pallet = pallet::Pallet::new(tcx);
    println!(" Done");

    println!("The following dispatchables will be analyzed :");
    analysis_utils::dispatchables_getter::print_dispatchable_names(tcx, &pallet.dispatchables);

    // Storage calls analysis
    /*for dispatchable_def_id in pallet.dispatchables.keys() {
        let storage_calls_analysis =
            storage_calls_analysis::StorageCallsAnalysis::new(tcx, &pallet);

        let mir = tcx.optimized_mir(dispatchable_def_id);
        let mut results = storage_calls_analysis
            .into_engine(tcx, mir)
            .pass_name("storage_calls_analysis")
            .iterate_to_fixpoint()
            .into_results_cursor(mir);

        let state =
            if let Some((last, _)) = rustc_middle::mir::traversal::reverse_postorder(mir).last() {
                results.seek_to_block_end(last);
                Some(results.get().clone())
            } else {
                None
            };

        println!(
            "{} --- {:?}",
            tcx.def_path_str(*dispatchable_def_id),
            state.unwrap().storage_accesses()
        );
        println!();
    }*/

    // Reads/Writes count anaylsis
    for dispatchable_def_id in pallet.dispatchables.keys() {

        let mir = tcx.optimized_mir(dispatchable_def_id);
        // Detect loops in analyzed function
        if mir.is_cfg_cyclic() {
            println!("Loop detected in function {}, loops are not supported", tcx.def_path_str(*dispatchable_def_id));
            continue;
        }

        let r_w_count_analysis = r_w_count_analysis::RWCountAnalysis::new(tcx, &pallet);

        let mut results = r_w_count_analysis
            .into_engine(tcx, mir)
            .pass_name("r_w_count_analysis")
            .iterate_to_fixpoint()
            .into_results_cursor(mir);

        if !*results.analysis().is_success.borrow() {
            println!("Analysis failed for {}", tcx.def_path_str(*dispatchable_def_id));
            continue;
        }

        let state =
            if let Some((last, _)) = rustc_middle::mir::traversal::reverse_postorder(mir).last() {
                results.seek_to_block_end(last);
                Some(results.get().clone())
            } else {
                None
            };

        println!(
            "{} --- {:?}",
            tcx.def_path_str(*dispatchable_def_id),
            state.unwrap()
        );
        println!();
    }
}
