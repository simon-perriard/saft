use crate::analysis_utils::extrinsics_getter::get_call_enum_variants_hir_ids;
use crate::analysis_utils::extrinsics_getter::get_dispatch_bypass_filter_local_def_id;
use crate::analysis_utils::extrinsics_getter::get_extrinsics_fn_ids;
use crate::types::Type;
use rustc_hir::def::Res;
use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::DefId;
use std::collections::HashMap;
use std::collections::HashSet;

pub struct Function {
    pub def_id: DefId,
}

pub struct Pallet {
    pub fields: HashMap<DefId, Field>,
    pub functions: HashMap<DefId, Function>,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub def_id: DefId,
    pub kind: StorageKind,
}

#[derive(Debug, Clone)]
pub enum StorageKind {
    StorageValue {
        value_type: Type,
    },
    StorageMap {
        key_type: Type,
        value_type: Type,
    },
    StorageDoubleMap {
        key1_type: Type,
        key2_type: Type,
        value_type: Type,
    },
    StorageNMap {
        key_type: Type,
        value_type: Type,
    },
    CountedStorageMap {
        key_type: Type,
        value_type: Type,
    },
}

impl Pallet {
    pub fn new(tcx: TyCtxt) -> Self {
        Pallet {
            fields: get_fields(tcx),
            functions: get_functions(tcx),
        }
    }
}

fn get_field_names<'tcx>(tcx: TyCtxt<'tcx>) -> HashSet<&'tcx str> {
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

fn get_fields(tcx: TyCtxt) -> HashMap<DefId, Field> {
    let field_names = get_field_names(tcx);
    let mut fields = HashMap::new();

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

            match tcx.def_path_str(*frame_storage_def_id).as_str() {
                "frame_support::pallet_prelude::StorageValue" => {
                if let rustc_hir::GenericArg::Type(value_hir_ty) = &args[1] {
                    fields.insert(alias_def_id, Field {
                        def_id: alias_def_id,
                        kind: StorageKind::StorageValue {
                            value_type: hir_ty_to_type(tcx, value_hir_ty),
                        }
                    });
                }
                },
                "frame_support::pallet_prelude::StorageMap" => {
                if let rustc_hir::GenericArg::Type(key_hir_ty) = &args[2]
                && let rustc_hir::GenericArg::Type(value_hir_ty) = &args[3] {
                    
                    fields.insert(alias_def_id, Field {
                        def_id: alias_def_id,
                        kind: StorageKind::StorageMap {
                            key_type: hir_ty_to_type(tcx, key_hir_ty),
                            value_type: hir_ty_to_type(tcx, value_hir_ty),
                        }
                    });
                }
                },
                "frame_support::pallet_prelude::StorageDoubleMap" => {
                if let rustc_hir::GenericArg::Type(key1_hir_ty) = &args[2]
                && let rustc_hir::GenericArg::Type(key2_hir_ty) = &args[4]
                && let rustc_hir::GenericArg::Type(value_hir_ty) = &args[5] {
                    
                    fields.insert(alias_def_id, Field {
                        def_id: alias_def_id,
                        kind: StorageKind::StorageDoubleMap {
                            key1_type: hir_ty_to_type(tcx, key1_hir_ty),
                            key2_type: hir_ty_to_type(tcx, key2_hir_ty),
                            value_type: hir_ty_to_type(tcx, value_hir_ty),
                        }
                    });
                }
                },
                "frame_support::pallet_prelude::StorageNMap" => {
                    if let rustc_hir::GenericArg::Type(key_hir_ty) = &args[1]
                    && let rustc_hir::GenericArg::Type(value_hir_ty) = &args[2] {
                        
                        fields.insert(alias_def_id, Field {
                            def_id: alias_def_id,
                            kind: StorageKind::StorageNMap {
                                key_type: hir_ty_to_type(tcx, key_hir_ty),
                                value_type: hir_ty_to_type(tcx, value_hir_ty),
                            }
                        });
                    }
                },
                "frame_support::pallet_prelude::CountedStorageMap" => {
                    if let rustc_hir::GenericArg::Type(key_hir_ty) = &args[2]
                    && let rustc_hir::GenericArg::Type(value_hir_ty) = &args[3] {
                        
                        fields.insert(alias_def_id, Field {
                            def_id: alias_def_id,
                            kind: StorageKind::CountedStorageMap {
                                key_type: hir_ty_to_type(tcx, key_hir_ty),
                                value_type: hir_ty_to_type(tcx, value_hir_ty),
                            }
                        });
                    }
                },
                _ => unreachable!()
            };
        }
    }
    fields
}

fn get_functions(tcx: TyCtxt) -> HashMap<DefId, Function> {
    // Retrieve the variants of the Call enum, aka names of extrinsics
    let variant_ids = get_call_enum_variants_hir_ids(tcx);
    // Retrieve local def id of the 'dispatch_bypass_filter' function, aka the function that
    // dispatches the calls at the pallet level
    let dispatch_local_def_id = get_dispatch_bypass_filter_local_def_id(tcx);

    let extrinsics_def_ids = if let Some(dispatch_local_def_id) = dispatch_local_def_id {
        get_extrinsics_fn_ids(tcx, dispatch_local_def_id, &variant_ids)
    } else {
        panic!("Pallet level dispatch function not found.\nFunction 'dispatch_bypass_filter' not found, are you running SAFT on the pallet level?");
    };

    let mut functions = HashMap::new();

    for function_def_id in extrinsics_def_ids {
        functions.insert(function_def_id, Function{def_id: function_def_id});
    }

    functions
}
