use rustc_middle::ty::TyCtxt;

use crate::{typesystem_common::*, typesystem_storage::get_storage_variables_names};

#[derive(Debug)]
pub struct PalletDeclaredType {
    pub alias_ident: Identifier,
    pub value: ValueType,
}

impl TypeSize for PalletDeclaredType {
    fn get_size(&self) -> CompSize {
        self.value.get_size()
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
                    value: explore(tcx, ty, ts),
                };

                ts.add_type(TypeVariant::PalletDeclaredType(standard_type), tcx)
            }
        }
    }
}
