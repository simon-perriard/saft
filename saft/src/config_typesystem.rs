use rustc_middle::ty::TyCtxt;

use crate::{storage_typesystem::get_storage_variables_names, analysis_utils::def_id_printer::get_def_id_name_with_path};

pub struct FrameConfigType {

}



pub fn get_config_types(tcx: &TyCtxt) {

    let storage_variables_names = get_storage_variables_names(tcx);

    for item in tcx.hir().items() {
        let rustc_hir::Item { ident, kind, def_id, .. } = item;

        if let rustc_hir::ItemKind::TyAlias(ty, generics) = kind {
            if !storage_variables_names.contains(&String::from(ident.as_str())) {
                // Types declared outside #[pallet::config] and are not #[pallet::storage]
                println!("{}", ident.as_str());
            }
        }

        if get_def_id_name_with_path(*tcx, def_id.to_def_id()) == "pallet::Config" 
        && let rustc_hir::ItemKind::Trait(_, _, _, _, items) = kind {
            // Types declared inside #[pallet::config]
            for item in items.iter() {
                if let rustc_hir::TraitItemRef {
                    ident,
                    kind: rustc_hir::AssocItemKind::Type,
                    ..
                } = item {
                    println!("{}", ident.as_str());
                }
            }
        }
    }
}

