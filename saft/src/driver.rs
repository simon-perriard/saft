use crate::pallet::Field;
use crate::types::Type;
use rustc_hir::def::Res;
use rustc_hir::def_id::DefId;
use rustc_middle::ty::TyCtxt;
use std::collections::HashSet;

use crate::pallet::StorageKind;

pub fn get_field_names<'tcx>(tcx: TyCtxt<'tcx>) -> HashSet<&'tcx str> {
    tcx.hir()
        .items()
        .filter_map(|item| {
            item.ident
                .as_str()
                .strip_prefix("_GeneratedPrefixForStorage")
        })
        .collect()
}

fn get_type_alias_data<'hir>(
    item_kind: &'hir rustc_hir::ItemKind<'hir>,
) -> Option<(&'hir DefId, &'hir [rustc_hir::GenericArg<'hir>])> {
    if let rustc_hir::ItemKind::TyAlias(ty, _) = item_kind
    && let rustc_hir::Ty { kind, .. } = ty
    && let rustc_hir::TyKind::Path(qpath) = kind
    && let rustc_hir::QPath::Resolved(_, path) = qpath
    && let rustc_hir::Path { res, segments, .. } = path
    && let rustc_hir::PathSegment { args, .. } = segments[0]
    && let Some(rustc_hir::GenericArgs { args, .. }) = args
    && let Res::Def(_, def_id) = res {
        Some((def_id, args))
    } else {
        None
    }
}

fn hir_ty_to_type(tcx: TyCtxt, hir_ty: &rustc_hir::Ty) -> Type {
    Type::from_mir_ty(tcx, rustc_typeck::hir_ty_to_ty(tcx, hir_ty))
}

/// Find the pallet's storage variables, resolve their types and try to get
/// their size. If size cannot be inferred, a symbol will be used instead.
pub fn get_fields(tcx: TyCtxt) {
    let field_names = get_field_names(tcx);

    for item in tcx.hir().items() {
        let rustc_hir::Item {
            ident,
            def_id,
            kind,
            ..
        } = item;

        if field_names.contains(&ident.as_str())
            && let Some((frame_storage_def_id, args)) = get_type_alias_data(kind)
        {
            let alias_def_id = def_id.to_def_id();

            let f = match tcx.def_path_str(*frame_storage_def_id).as_str() {
                "frame_support::pallet_prelude::StorageValue" => {
                if let rustc_hir::GenericArg::Type(value_hir_ty) = &args[1] {
                    Field {
                        def_id: alias_def_id,
                        kind: StorageKind::StorageValue {
                            value_type: hir_ty_to_type(tcx, value_hir_ty),
                        }
                    }
                } else {
                    unreachable!();
                }
                },
                "frame_support::pallet_prelude::StorageMap" => {
                if let rustc_hir::GenericArg::Type(key_hir_ty) = &args[2]
                && let rustc_hir::GenericArg::Type(value_hir_ty) = &args[3] {
                    Field {
                        def_id: alias_def_id,
                        kind: StorageKind::StorageMap {
                            key_type: hir_ty_to_type(tcx, key_hir_ty),
                            value_type: hir_ty_to_type(tcx, value_hir_ty),
                        }
                    }
                } else {
                    unreachable!();
                }
                },
                "frame_support::pallet_prelude::StorageDoubleMap" => {
                if let rustc_hir::GenericArg::Type(key1_hir_ty) = &args[2]
                && let rustc_hir::GenericArg::Type(key2_hir_ty) = &args[4]
                && let rustc_hir::GenericArg::Type(value_hir_ty) = &args[5] {
                    Field {
                        def_id: alias_def_id,
                        kind: StorageKind::StorageDoubleMap {
                            key1_type: hir_ty_to_type(tcx, key1_hir_ty),
                            key2_type: hir_ty_to_type(tcx, key2_hir_ty),
                            value_type: hir_ty_to_type(tcx, value_hir_ty),
                        }
                    }
                } else {
                    unreachable!();
                }
                },
                "frame_support::pallet_prelude::StorageNMap" => {
                    if let rustc_hir::GenericArg::Type(key_hir_ty) = &args[1]
                    && let rustc_hir::GenericArg::Type(value_hir_ty) = &args[2] {
                        Field {
                            def_id: alias_def_id,
                            kind: StorageKind::StorageNMap {
                                key_type: hir_ty_to_type(tcx, key_hir_ty),
                                value_type: hir_ty_to_type(tcx, value_hir_ty),
                            }
                        }
                    } else {
                        unreachable!();
                    }
                },
                "frame_support::pallet_prelude::CountedStorageMap" => {
                    if let rustc_hir::GenericArg::Type(key_hir_ty) = &args[2]
                    && let rustc_hir::GenericArg::Type(value_hir_ty) = &args[3] {
                        Field {
                            def_id: alias_def_id,
                            kind: StorageKind::CountedStorageMap {
                                key_type: hir_ty_to_type(tcx, key_hir_ty),
                                value_type: hir_ty_to_type(tcx, value_hir_ty),
                            }
                        }
                    } else {
                        unreachable!();
                    }
                },
                _ => unreachable!()
            };

            println!("{:?}", f);
        }
    }
}
