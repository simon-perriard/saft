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
extern crate rustc_target;
extern crate rustc_typeck;

pub mod analysis;
pub mod sysroot;
use std::collections::HashMap;

use crate::analysis::{cost_analysis::AnalysisState, *};
use rustc_mir_dataflow::Analysis;

pub fn extract_juice(tcx: rustc_middle::ty::TyCtxt) {
    // Extract pallet
    print!("Extracting pallet information...");
    let pallet = pallet::Pallet::new(tcx);
    println!(" Done");

    print!("Extracting event variants...");
    let mut event_variants = HashMap::new();

    // Analysis to extract the variant of each deposited event
    for crate_def_id in tcx
        .hir()
        .body_owners()
        .filter(|body_owner| tcx.hir().body_owner_kind(*body_owner).is_fn_or_closure())
        .map(|local_def_id| local_def_id.to_def_id())
    {
        let mir = tcx.optimized_mir(crate_def_id);

        if mir.is_cfg_cyclic() {
            // We simply do not analyze if there is a loop, the user will be alerted anyway during the cost analysis
            continue;
        }

        let events_variants_analysis =
            events_variants_analysis::EventsVariantsAnalysis::new(tcx, mir);
        let mut results = events_variants_analysis
            .into_engine(tcx, mir)
            .pass_name("events_variants_analysis")
            .iterate_to_fixpoint()
            .into_results_cursor(mir);

        if let Some((last, _)) = rustc_middle::mir::traversal::reverse_postorder(mir).last() {
            results.seek_to_block_end(last);
            let end_state = results.get().clone();

            if !end_state.is_empty() {
                event_variants.insert(crate_def_id, end_state);
            }
        }
    }
    let event_variants = event_variants;
    println!(" Done");

    println!("The following dispatchables will be analyzed :");
    analysis_utils::dispatchables_getter::print_dispatchable_names(tcx, &pallet.dispatchables);
    println!();

    // Reads/Writes count anaylsis
    for dispatchable_def_id in pallet.dispatchables.keys() {
        let mir = tcx.optimized_mir(dispatchable_def_id);
        // Detect loops in analyzed function
        if mir.is_cfg_cyclic() {
            println!(
                "Loop detected in function {}, loops are not supported",
                tcx.def_path_str(*dispatchable_def_id)
            );
            println!(
                "Analysis failed for {}",
                tcx.def_path_str(*dispatchable_def_id)
            );
            println!();
            continue;
        }

        let cost_analysis =
            cost_analysis::CostAnalysis::new(tcx, &pallet, &event_variants, *dispatchable_def_id);

        let mut results = cost_analysis
            .into_engine(tcx, mir)
            .pass_name("cost_analysis")
            .iterate_to_fixpoint()
            .into_results_cursor(mir);

        if *results.analysis().analysis_state.borrow() == AnalysisState::Failure {
            println!(
                "Analysis failed for {}",
                tcx.def_path_str(*dispatchable_def_id)
            );
            println!();
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
            "****************\nSummary for dispatchable {}\n{}****************",
            tcx.def_path_str(*dispatchable_def_id),
            state.unwrap()
        );
        println!();
    }
}
