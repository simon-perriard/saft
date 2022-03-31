use rustc_middle::ty::TyCtxt;

use crate::typesystem_common::*;
use crate::typesystem_types::struct_type::Struct;

use super::typesystem_storage::get_storage_variables_names;

#[derive(Debug)]
pub struct PalletDeclaredType {
    pub alias_ident: Identifier,
    pub value: Type,
}

impl Alias for PalletDeclaredType {
    fn get_size(&self) -> SizeType {
        self.value.collect_size()
    }

    fn get_name(&self, tcx: &TyCtxt) -> String {
        self.alias_ident.get_name(tcx)
    }

    fn get_name_full(&self, tcx: &TyCtxt) -> String {
        self.alias_ident.get_name_full(tcx)
    }
}

pub fn get_declared_types(tcx: &TyCtxt, ts: &mut TySys) {
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
                    def_id: def_id.to_def_id(),
                };

                let standard_type = PalletDeclaredType {
                    alias_ident,
                    value: explore(tcx, ty, ts).expect_type(),
                };
                //println!("{:?}", standard_type);
                //println!("");
                ts.add_type(TypeVariant::PalletDeclaredType(standard_type), tcx)
            }
        } else if let rustc_hir::ItemKind::Struct(variant_data, _) = kind {
            let alias_ident = Identifier {
                def_id: def_id.to_def_id(),
            };

            match variant_data {
                rustc_hir::VariantData::Struct(field_defs, _) => {
                    let mut members = Vec::new();

                    for field_def in field_defs.iter() {
                        members.push((
                            explore(tcx, field_def.ty, ts).expect_type(),
                            String::from(field_def.ident.as_str()),
                        ));
                    }

                    let standard_type = PalletDeclaredType {
                        alias_ident,
                        value: Type::Struct(Struct::new(members)),
                    };

                    ts.add_type(TypeVariant::PalletDeclaredType(standard_type), tcx)
                }
                rustc_hir::VariantData::Tuple(_, _) => (),
                rustc_hir::VariantData::Unit(_) => (),
            }
        }
    }
}
