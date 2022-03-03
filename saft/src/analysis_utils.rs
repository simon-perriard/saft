use rustc_middle::ty::TyCtxt;
use rustc_hir::def_id::{DefId,LocalDefId};
use rustc_hir::hir_id::HirId;

pub fn get_fn_name(tcx: TyCtxt, def_id: DefId) -> String {
    
    let full_name = get_fn_name_with_path(tcx, def_id);

    let split = full_name.split_terminator("::").collect::<Vec<_>>();
    let splat = *split.last().unwrap_or_else(|| &"");

    if  splat == "" ||
        splat == "_" ||
        splat.starts_with("{") && splat.ends_with('}') {
            return String::new();
    }

    splat.to_string()
}

pub fn get_fn_name_with_path(tcx: TyCtxt, def_id: DefId) -> String {

    tcx.def_path_str(def_id)
}

pub fn get_call_enum_variants_hir_ids(tcx: TyCtxt) -> Vec<&HirId> {

    let mut ids: Vec<&HirId> = Vec::new();

    for item in tcx.hir().items() {
        match item {
            rustc_hir::Item { ident, kind, vis, .. } => {
                if vis.node.is_pub() {
                    match kind {
                        rustc_hir::ItemKind::Enum(enum_def, _) => {
                            if ident.as_str() == "Call" {
                                for variant in enum_def.variants.iter() {
                                   match variant {
                                       rustc_hir::Variant { ident, id, .. } => {
                                            if ident.as_str() != "__Ignore" {
                                                ids.push(id);
                                            }
                                       }
                                   }
                                }
                            }
                        }
                        _ => ()
                    }
                }
            }
        }
    }

    ids
}

pub fn print_extrinsics_names(tcx: TyCtxt, ids: Option<Vec<&HirId>>) {

    let mut ids_lst: Vec<&HirId> = Vec::new();

    if let Some(ids) = ids {
        ids_lst = ids;
    } else {
        ids_lst = get_call_enum_variants_hir_ids(tcx);
    }

    for id in ids_lst {
        if let Some(ident) = tcx.hir().get(*id).ident() {
            println!("{}",ident.as_str());
        }
        
    }
}

pub fn get_extrinsics_ids_WEAK(tcx: TyCtxt, ids: &Vec<&HirId>) -> Vec<LocalDefId>{

    let mut res = Vec::new();

    for local_def_id in tcx.hir().body_owners() {
        let def_id = local_def_id.to_def_id();

        for id in ids {
            let fn_name = get_fn_name(tcx, def_id);
            if let Some(ident) = tcx.hir().get(**id).ident() {
                if ident.as_str() == fn_name {
                    res.push(local_def_id);
                    if res.len() == ids.len() {
                        return res;
                    }
                }
            }
        }
    }

    res
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
pub fn get_extrinsics_fn_ids(tcx: TyCtxt, dispatch_local_def_id: LocalDefId, variant_ids: &Vec<&HirId>) {

    let dispatch_def_hir_id = tcx.hir().local_def_id_to_hir_id(dispatch_local_def_id);

    for variant_id in variant_ids {

        let body_owner = tcx.hir().body_owned_by(dispatch_def_hir_id);
        let body = tcx.hir().body(body_owner);

        let match_target = tcx.hir().get(**variant_id).ident().unwrap();

        match body {
            rustc_hir::Body{ value, ..} => {
                let called_fn_path = go_down_dispatch_bypass_filter(&value.kind, match_target.as_str());
                if let Some((hir_id, qpath)) = called_fn_path {

                    for parent in tcx.hir().parent_owner_iter(dispatch_def_hir_id) {
                        println!("HEY");
                        let body_id = tcx.hir().body_owned_by(tcx.hir().local_def_id_to_hir_id(parent.0));
                        let typeck_results = tcx.typeck_body(body_id);
                        let def_id_one = typeck_results.type_dependent_def_id(*hir_id);
                        let res = typeck_results.qpath_res(qpath, *hir_id);

                        match res {
                            rustc_hir::def::Res::Def(def_kind, def_id) => {
                                println!("{:?}, {:?}", def_id_one, def_id);
                                break
                            },

                            _ => (
                                println!("{:?}, {:?}", res, def_id_one)
                            )
                        }
                    }
                } else {
                    println!("function '{}' not found", match_target.as_str());
                }
            }
        }

    }
}

pub fn go_down_dispatch_bypass_filter<'a>(current_node: &'a  rustc_hir::ExprKind, match_target: &'a  str) -> Option<(&'a HirId, &'a rustc_hir::QPath<'a>)>{

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
                match arm {
                    rustc_hir::Arm { pat, body, .. } => {

                        if is_matching(&pat.kind, match_target) {
                            return go_down_dispatch_bypass_filter(&body.kind, match_target);
                        } else {
                            continue
                        }
                    }
                }
            }
            None
        }

        rustc_hir::ExprKind::MethodCall(_, exprs, _) => {

            match exprs[0].kind {

                rustc_hir::ExprKind::Call(expr, _) => {
                    match &expr.kind {
                        rustc_hir::ExprKind::Path(qpath) => {
                            Some((&exprs[0].hir_id, &qpath))
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

pub fn is_matching(pattern: &rustc_hir::PatKind, match_target: &str) -> bool {
    match pattern {
        rustc_hir::PatKind::Struct(qpath, _, _) => {
            match qpath {
                rustc_hir::QPath::TypeRelative(_, path_segment) => {
                   path_segment.ident.as_str() == match_target
                }
                _ => false
            }
        }
        _ => false
    }
}
