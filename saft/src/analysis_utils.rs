use rustc_middle::ty::TyCtxt;
use rustc_hir::def_id::{DefId,LocalDefId};
use rustc_hir::Node;
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