use rustc_middle::ty::TyCtxt;

use crate::{
    analysis_utils::def_id_printer::*, typesystem_common::*,
    typesystem_storage::get_storage_variables_names,
};

pub enum PalletStandardType {
    TyAlias {
        alias_ident: Identifier,
        value: ValueType,
    },
    TyBounds {
        alias_ident: Identifier,
        trait_bounds: Vec<ValueType>,
    },
}

impl TypeSize for PalletStandardType {
    fn get_size(&self) -> CompSize {
        match self {
            PalletStandardType::TyAlias { value, .. } => value.get_size(),
            PalletStandardType::TyBounds { trait_bounds, .. } => {
                if trait_bounds.is_empty() {
                    unreachable!("Trait bounds cannot be empty");
                }

                if trait_bounds.len() == 1 {
                    trait_bounds[0].get_size()
                } else {
                    unreachable!();
                }
            }
        }
    }

    fn get_name(&self) -> String {
        match self {
            PalletStandardType::TyAlias { alias_ident, .. }
            | PalletStandardType::TyBounds { alias_ident, .. } => alias_ident.name_short.clone(),
        }
    }

    fn get_name_full(&self) -> String {
        match self {
            PalletStandardType::TyAlias { alias_ident, .. }
            | PalletStandardType::TyBounds { alias_ident, .. } => alias_ident.name_full.clone(),
        }
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
                let alias_ident = Identifier {
                    name_short: get_def_id_name(*tcx, def_id.to_def_id()),
                    name_full: get_def_id_name_with_path(*tcx, def_id.to_def_id()),
                };

                let standard_type = PalletStandardType::TyAlias {
                    alias_ident,
                    value: explore(tcx, ty),
                };

                ts.add_type(TypeVariant::PalletStandardType(standard_type))
            }
        }
    }

    for item in tcx.hir().items() {
        let rustc_hir::Item { kind, def_id, .. } = item;

        if get_def_id_name_with_path(*tcx, def_id.to_def_id()) == "pallet::Config" 
        && let rustc_hir::ItemKind::Trait(_, _, _, _, items_refs) = kind {

            // Types declared inside #[pallet::config]
            for items_ref in items_refs.iter() {

                if let rustc_hir::TraitItemRef {
                        id,
                        kind: rustc_hir::AssocItemKind::Type,
                        ..
                    } = items_ref
                    && let rustc_hir::TraitItemKind::Type(generic_bounds, _) = tcx.hir().expect_trait_item(id.def_id).kind
                {
                    let alias_ident = Identifier {
                        name_short: get_def_id_name(*tcx, id.def_id.to_def_id()),
                        name_full: get_def_id_name_with_path(*tcx, id.def_id.to_def_id()),
                    };
                    let mut trait_bounds = Vec::new();

                    for generic_bound in generic_bounds.iter() {
                        if let rustc_hir::GenericBound::Trait(poly_trait_ref, _) = generic_bound
                        && let rustc_hir::PolyTraitRef { trait_ref, .. } = poly_trait_ref {
                            trait_bounds.push(get_value_type(tcx, trait_ref.path));
                        }
                    }

                    let standard_type = PalletStandardType::TyBounds {
                        alias_ident,
                        trait_bounds
                    };
                    ts.add_type(TypeVariant::PalletStandardType(standard_type))
                }
            }
        }
    }
}
