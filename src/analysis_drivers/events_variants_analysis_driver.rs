use crate::analysis::{events_variants_analysis, events_variants_domain::EventVariantsDomain};
use rustc_middle::ty::TyCtxt;
use rustc_mir_dataflow::Analysis;
use rustc_span::def_id::DefId;
use std::collections::HashMap;

pub(crate) fn events_variants_analysis(tcx: TyCtxt) -> HashMap<DefId, EventVariantsDomain> {
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

    event_variants
}
