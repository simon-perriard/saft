use rustc_middle::ty::TyCtxt;

use crate::{
    analysis_utils::def_id_printer::*, typesystem_common::*,
    typesystem_storage::get_storage_variables_names,
};

pub struct PalletStandardType {
    pub alias_ident: Ident,
    pub value: ValueType,
}

impl TypeSize for PalletStandardType {
    fn get_size(&self) -> CompSize {
        self.value.get_size()
    }

    fn get_name(&self) -> String {
        self.alias_ident.name_short.clone()
    }

    fn get_name_full(&self) -> String {
        self.alias_ident.name_full.clone()
    }
}

pub fn get_config_types(tcx: &TyCtxt, ts: &mut TySys) {
    let storage_variables_names = get_storage_variables_names(tcx);

    // Start with outermost type declaration as they may be used
    // for other type def in the Config trait
    for item in tcx.hir().items() {
        let rustc_hir::Item {
            ident,
            kind,
            def_id,
            ..
        } = item;

        if let rustc_hir::ItemKind::TyAlias(ty, _) = kind {
            if !storage_variables_names.contains(&String::from(ident.as_str())) {
                // Types declared outside #[pallet::config] and are not #[pallet::storage]
                let alias_ident = Ident {
                    name_short: get_def_id_name(*tcx, def_id.to_def_id()),
                    name_full: get_def_id_name_with_path(*tcx, def_id.to_def_id()),
                };

                let standard_type = PalletStandardType {
                    alias_ident,
                    value: explore(tcx, ty),
                };

                ts.add_type(TypeVariant::PalletStandardType(standard_type))
            }
        }
    }

    for item in tcx.hir().items() {
        let rustc_hir::Item {
            ident,
            kind,
            def_id,
            ..
        } = item;

        if get_def_id_name_with_path(*tcx, def_id.to_def_id()) == "pallet::Config" 
        && let rustc_hir::ItemKind::Trait(_, _, _, _, items_refs) = kind {
            // Types declared inside #[pallet::config]
            for items_refs in items_refs.iter() {
                if let rustc_hir::TraitItemRef {
                    ident,
                    kind: rustc_hir::AssocItemKind::Type,
                    ..
                } = items_refs {
                    println!("{:?}", ident.as_str());
                }
            }
        }
    }
}
