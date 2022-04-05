//! Helper module specialized in building the extended typesystem for the storage variables
use rustc_hir::def::Res;
use rustc_hir::def_id::LocalDefId;
use rustc_middle::ty::TyCtxt;

use crate::{analysis_utils::def_id_printer::*, typesystem_common::*};
#[derive(Clone, Debug)]
pub struct FrameStorageType {
    pub alias_ident: Identifier,
    pub kind: StorageKind,
}

#[derive(Clone, Debug)]
/// Root types for Substrate storage
/// `<https://docs.substrate.io/rustdocs/latest/frame_support/pallet_prelude/index.html#structs>`
pub enum StorageKind {
    StorageValue {
        ident: Identifier,
        value: crate::typesystem_common::Type,
    },
    StorageMap {
        ident: Identifier,
        value: crate::typesystem_common::Type,
        //max_values: Option<ValueType>,
    },
    StorageDoubleMap {
        ident: Identifier,
        value: crate::typesystem_common::Type,
        //max_values: Option<ValueType>,
    },
    StorageNMap {
        ident: Identifier,
        value: crate::typesystem_common::Type,
        //max_values: Option<ValueType>,
    },
    CountedStorageMap {
        ident: Identifier,
        value: crate::typesystem_common::Type,
        //max_values: Option<ValueType>,
    },
}

impl FrameStorageType {
    pub fn new(local_def_id: LocalDefId, kind: StorageKind) -> Self {
        FrameStorageType {
            alias_ident: Identifier {
                def_id: local_def_id.to_def_id(),
            },
            kind,
        }
    }
}

impl Alias for FrameStorageType {
    fn get_size(&self) -> SizeType {
        match &self.kind {
            StorageKind::StorageValue { value, .. } => value.collect_size(),
            StorageKind::StorageMap { value, .. } => value.collect_size(),
            StorageKind::StorageDoubleMap { value, .. } => value.collect_size(),
            StorageKind::StorageNMap { value, .. } => value.collect_size(),
            StorageKind::CountedStorageMap { value, .. } => value.collect_size(),
        }
    }

    fn get_name(&self, tcx: &TyCtxt) -> String {
        self.alias_ident.get_name(tcx)
    }

    fn get_name_full(&self, tcx: &TyCtxt) -> String {
        self.alias_ident.get_name_full(tcx)
    }
}

pub fn get_storage_variables_names(tcx: &TyCtxt) -> Vec<String> {
    let mut storage_variables_names = Vec::new();

    for item in tcx.hir().items() {
        let rustc_hir::Item { ident, .. } = item;

        // _GeneratedPrefixForStorageNAME is generated for storage variables
        if ident.as_str().contains("_GeneratedPrefixForStorage") {
            storage_variables_names.push(ident.as_str().replace("_GeneratedPrefixForStorage", ""));
        }
    }

    storage_variables_names
}

/// Find the pallet's storage variables, resolve their types and try to get
/// their size. If size cannot be inferred, a symbol will be used instead.
pub fn get_storage_variables<'ts>(tcx: TyCtxt<'ts>, mut ts: TySys<'ts>) -> TySys<'ts> {
    let storage_variables_names = get_storage_variables_names(&tcx);

    for item in tcx.hir().items() {
        let rustc_hir::Item {
            ident,
            def_id,
            kind,
            ..
        } = item;
        if storage_variables_names.contains(&String::from(ident.as_str()))
            && let rustc_hir::ItemKind::TyAlias(ty, _) = kind
            && let rustc_hir::Ty { kind, .. } = ty
            && let rustc_hir::TyKind::Path(qpath) = kind
            && let rustc_hir::QPath::Resolved(_, path) = qpath
            && let rustc_hir::Path { res, segments, .. } = path
            && let rustc_hir::PathSegment { args, .. } = segments[0]
            && let Some(rustc_hir::GenericArgs { args, .. }) = args
        {

            let ty_ty: rustc_middle::ty::Ty = tcx.type_of(*def_id);
            println!("{:?}", ty_ty);

            let kind = if let Res::Def(_, def_id) = res {
                let ident = Identifier {
                    def_id: *def_id
                };
                match get_def_id_name_with_path(tcx, *def_id).as_str() {
                        "frame_support::pallet_prelude::StorageValue" => {
                        if let rustc_hir::GenericArg::Type(ty) = &args[1] {
                            StorageKind::StorageValue {
                                ident,
                                value: explore(&tcx, ty, &ts).expect_type(),
                            }
                            } else {unreachable!()}
                        },
                        "frame_support::pallet_prelude::StorageMap" => {
                        if let rustc_hir::GenericArg::Type(ty) = &args[3] {
                            StorageKind::StorageNMap {
                                ident,
                                value: explore(&tcx, ty, &ts).expect_type(),
                                //max_values: Some(explore(tcx, &args[6]))
                            }
                            } else {unreachable!()}
                        },
                        "frame_support::pallet_prelude::StorageDoubleMap" => {
                        if let rustc_hir::GenericArg::Type(ty) = &args[3] {
                            StorageKind::StorageNMap {
                                ident,
                                value: explore(&tcx, ty, &ts).expect_type(),
                                //max_values: Some(explore(tcx, &args[6]))
                            }
                            } else {unreachable!()}

                        },
                        "frame_support::pallet_prelude::StorageNMap" => {
                        if let rustc_hir::GenericArg::Type(ty) = &args[3] {
                            StorageKind::StorageNMap {
                                ident,
                                value: explore(&tcx, ty, &ts).expect_type(),
                                //max_values: Some(explore(tcx, &args[6]))
                            }
                            } else {unreachable!()}

                        },
                        "frame_support::pallet_prelude::CountedStorageMap" => {
                        if let rustc_hir::GenericArg::Type(ty) = &args[3] {
                            StorageKind::CountedStorageMap {
                                ident,
                                value: explore(&tcx, ty, &ts).expect_type(),
                                //max_values: Some(explore(tcx, &args[6]))
                            }
                            } else {unreachable!()}

                        },
                        _ => {unreachable!()}
                    }
            } else {
                unreachable!();
            };

            let storage_type = FrameStorageType::new(*def_id, kind);
            //println!("{:?}", storage_type);
            //println!("");

            ts.add_type(ty_ty, TypeVariant::FrameStorageType(storage_type));
        }
    }

    ts
}
