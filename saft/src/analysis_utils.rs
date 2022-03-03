use rustc_middle::ty::TyCtxt;
use rustc_hir::def_id::{DefId,LocalDefId};
use rustc_hir::hir_id::HirId;

pub fn get_fn_name(tcx: TyCtxt, def_id: DefId) -> String {
    
    let full_name = get_fn_name_with_path(tcx, def_id);
    let split = full_name.split_terminator("::").collect::<Vec<_>>();
    let splat = *split.last().unwrap_or(&"");

    splat.to_string()
}

pub fn get_fn_name_with_path(tcx: TyCtxt, def_id: DefId) -> String {
    tcx.def_path_str(def_id)
}

pub fn get_call_enum_variants_hir_ids(tcx: TyCtxt) -> Vec<&HirId> {

    let mut ids: Vec<&HirId> = Vec::new();

    for item in tcx.hir().items() {
        let rustc_hir::Item { ident, kind, vis, .. } = item;
        if vis.node.is_pub() {
            if let rustc_hir::ItemKind::Enum(enum_def, _) = kind {
                if ident.as_str() == "Call" {
                    for variant in enum_def.variants.iter() {
                        let rustc_hir::Variant { ident, id, .. } = variant;
                            if ident.as_str() != "__Ignore" {
                                ids.push(id);
                            }
                    }
                }
            }
        }
        
    }
    ids
}

pub fn print_extrinsics_names(tcx: TyCtxt, ids: Option<Vec<&HirId>>) {

    let ids_lst = if let Some(ids) = ids {
        ids
    } else {
        get_call_enum_variants_hir_ids(tcx)
    };

    for id in ids_lst {
        if let Some(ident) = tcx.hir().get(*id).ident() {
            println!("{}",ident.as_str());
        }
        
    }
}

pub fn get_dispatch_bypass_filter_local_def_id(tcx: TyCtxt) -> Option<LocalDefId> {

    for local_def_id in tcx.hir().body_owners() {

        let def_id = local_def_id.to_def_id();
        let pallet_call_dispatch = "<pallet::Call<T> as frame_support::dispatch::UnfilteredDispatchable>::dispatch_bypass_filter";

        if get_fn_name_with_path(tcx, def_id) == pallet_call_dispatch {
            return Some(local_def_id)
        }
    }
    None
}

// path resolution qpath_res is tricky, this function does not work
pub fn get_extrinsics_fn_ids(tcx: TyCtxt, dispatch_local_def_id: LocalDefId, variant_ids: &Vec<&HirId>) -> Vec<DefId>{

    let mut extrinsics_fn_ids = Vec::new();
    let dispatch_def_hir_id = tcx.hir().local_def_id_to_hir_id(dispatch_local_def_id);

    for variant_id in variant_ids {

        let body_owner = tcx.hir().body_owned_by(dispatch_def_hir_id);
        let body = tcx.hir().body(body_owner);
        let match_target = tcx.hir().get(**variant_id).ident().unwrap();

            let rustc_hir::Body{ value, ..} = body; {
            let called_fn_path = go_down_dispatch_bypass_filter(&value.kind, match_target.as_str());
            if let Some((hir_id, qpath)) = called_fn_path {
                let typeck_results = tcx.typeck(tcx.hir().local_def_id(dispatch_def_hir_id));
                if let Some(def_id) = typeck_results.qpath_res(qpath, *hir_id).opt_def_id() {
                    extrinsics_fn_ids.push(def_id);
                } else {
                    println!("function '{}' not found", match_target.as_str());
                }
            } else {
                println!("function '{}' not found", match_target.as_str());
            }
        }

    }
    extrinsics_fn_ids
}

fn go_down_dispatch_bypass_filter<'a>(current_node: &'a  rustc_hir::ExprKind, match_target: &'a  str) -> Option<(&'a HirId, &'a rustc_hir::QPath<'a>)>{

    match current_node {
        rustc_hir::ExprKind::Block(block, _) => {
            if let Some(expr) = block.expr {
                go_down_dispatch_bypass_filter(&expr.kind, match_target)
            } else {
                None
            }
        },

        rustc_hir::ExprKind::Match(_, arms,_) => {
            for arm in *arms {
                let rustc_hir::Arm { pat, body, .. } = arm;
                if is_matching(&pat.kind, match_target) {
                    return go_down_dispatch_bypass_filter(&body.kind, match_target);
                } else {
                    continue
                }
            }
            None
        }

        rustc_hir::ExprKind::MethodCall(_, exprs, _) => {
            match exprs[0].kind {
                rustc_hir::ExprKind::Call(expr, _) => {
                    match &expr.kind {
                        rustc_hir::ExprKind::Path(qpath) => {
                            Some((&expr.hir_id, qpath))
                        },
                        _ => None
                    }
                }
                _ => go_down_dispatch_bypass_filter(&exprs[0].kind, match_target)
            }
        }
        _ => {
            None
        }
    }
}

fn is_matching(pattern: &rustc_hir::PatKind, match_target: &str) -> bool {
    match pattern {
        rustc_hir::PatKind::Struct(rustc_hir::QPath::TypeRelative(_, path_segment), _, _) => {
            path_segment.ident.as_str() == match_target
        }
        _ => false
    }
}
