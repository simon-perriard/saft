use crate::analysis::{
    cost_analysis::{self, AnalysisState},
    events_variants_domain::EventVariantsDomain,
    pallet::Pallet,
};
use rustc_middle::ty::TyCtxt;
use rustc_mir_dataflow::Analysis;
use rustc_span::def_id::DefId;
use std::collections::HashMap;

pub(crate) fn cost_analysis(
    tcx: TyCtxt,
    pallet: Pallet,
    events_variants: HashMap<DefId, EventVariantsDomain>,
) {
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
        println!("{}", tcx.def_path_str(*dispatchable_def_id));
        if !tcx
            .def_path_str(*dispatchable_def_id)
            .contains("set_balance")
        {
            //continue;
        }

        let cost_analysis =
            cost_analysis::CostAnalysis::new(tcx, &pallet, &events_variants, *dispatchable_def_id);

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

        /*println!(
            "********************************\nSummary for dispatchable {}\n{}********************************",
            tcx.def_path_str(*dispatchable_def_id),
            state.unwrap()
        );*/
        println!();
    }
}
