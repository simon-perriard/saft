use rustc_middle::ty::TyCtxt;

use crate::{
    analysis_utils::{def_id_printer::*, typesystem_helpers::get_pallet_constant_types_name},
    typesystem_common::*,
};

#[derive(Debug)]
pub struct PalletConfigConstantType {
    pub alias_ident: Identifier,
    pub trait_bound: ValueType,
}

impl TypeSize for PalletConfigConstantType {
    fn get_size(&self) -> CompSize {
        self.trait_bound.get_size()
    }

    fn get_name(&self) -> String {
        self.alias_ident.name_short.clone()
    }

    fn get_name_full(&self) -> String {
        self.alias_ident.name_full.clone()
    }
}

pub fn get_config_constant_types(tcx: &TyCtxt, ts: &mut TySys) {
    let constant_types_names = get_pallet_constant_types_name(tcx);

    for item in tcx.hir().items() {
        let rustc_hir::Item { kind, def_id, .. } = item;

        if get_def_id_name_with_path(*tcx, def_id.to_def_id()) == "pallet::Config" 
        && let rustc_hir::ItemKind::Trait(_, _, _, _, items_refs) = kind {

            // Types declared inside #[pallet::config]
            // that have the #[pallet::constant]
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

                    if constant_types_names.contains(&alias_ident.name_short) {

                        if generic_bounds.is_empty() {
                            panic!("Trait bounds cannot be empty.");
                        }

                        let trait_bound = if let rustc_hir::GenericBound::Trait(poly_trait_ref, _) = &generic_bounds[0]
                        && let rustc_hir::PolyTraitRef { trait_ref, .. } = poly_trait_ref {
                            get_value_type(tcx, trait_ref.path, ts)
                        } else {
                            unreachable!();
                        };

                        let standard_type = PalletConfigConstantType {
                            alias_ident,
                            trait_bound
                        };
                        ts.add_type(TypeVariant::PalletConfigConstantType(standard_type))
                    }
                }
            }
        }
    }
}
