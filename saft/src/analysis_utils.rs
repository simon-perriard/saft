pub mod def_id_printer {
    use rustc_hir::def_id::DefId;
    use rustc_middle::ty::TyCtxt;
    pub fn get_def_id_name(tcx: TyCtxt, def_id: DefId) -> String {
        let full_name = get_def_id_name_with_path(tcx, def_id);
        let split = full_name.split_terminator("::").collect::<Vec<_>>();
        let splat = *split.last().unwrap_or(&"");

        splat.to_string()
    }

    pub fn get_def_id_name_with_path(tcx: TyCtxt, def_id: DefId) -> String {
        tcx.def_path_str(def_id)
    }
}

pub mod extrinsics_getter {

    use regex::Regex;
    use rustc_hir::def_id::{DefId, LocalDefId};
    use rustc_hir::hir_id::HirId;
    use rustc_middle::ty::TyCtxt;

    use super::def_id_printer::get_def_id_name_with_path;

    pub fn get_call_enum_variants_hir_ids(tcx: TyCtxt) -> Vec<&HirId> {
        let mut ids: Vec<&HirId> = Vec::new();

        for item in tcx.hir().items() {
            let rustc_hir::Item {
                ident, kind, vis, ..
            } = item;
            if vis.node.is_pub() {
                if let rustc_hir::ItemKind::Enum(enum_def, _) = kind {
                    if ident.as_str() == "Call" {
                        for variant in enum_def.variants.iter() {
                            let rustc_hir::Variant { ident, id, .. } = variant;
                            if ident.as_str() != "__Ignore" {
                                ids.push(id);
                            }
                        }
                    }
                }
            }
        }
        ids
    }

    pub fn print_extrinsics_names(tcx: TyCtxt, ids: Option<Vec<&HirId>>) {
        let ids_lst = if let Some(ids) = ids {
            ids
        } else {
            get_call_enum_variants_hir_ids(tcx)
        };

        for id in ids_lst {
            if let Some(ident) = tcx.hir().get(*id).ident() {
                println!("{}", ident.as_str());
            }
        }
    }

    pub fn get_dispatch_bypass_filter_local_def_id(tcx: TyCtxt) -> Option<LocalDefId> {
        for local_def_id in tcx.hir().body_owners() {
            let def_id = local_def_id.to_def_id();
            let pallet_call_dispatch_regex = Regex::new(r"<pallet::Call<.*\s*(,.*)*> as frame_support::dispatch::UnfilteredDispatchable>::dispatch_bypass_filter").unwrap();

            if pallet_call_dispatch_regex.is_match(&get_def_id_name_with_path(tcx, def_id)) {
                return Some(local_def_id);
            }
        }
        None
    }

    pub fn get_extrinsics_fn_ids(
        tcx: TyCtxt,
        dispatch_local_def_id: LocalDefId,
        variant_ids: &Vec<&HirId>,
    ) -> Vec<DefId> {
        let mut extrinsics_fn_ids = Vec::new();
        let dispatch_def_hir_id = tcx.hir().local_def_id_to_hir_id(dispatch_local_def_id);

        for variant_id in variant_ids {
            let body_owner = tcx.hir().body_owned_by(dispatch_def_hir_id);
            let body = tcx.hir().body(body_owner);
            let match_target = tcx.hir().get(**variant_id).ident().unwrap();

            let rustc_hir::Body { value, .. } = body;
            {
                let called_fn_path =
                    go_down_dispatch_bypass_filter(&value.kind, match_target.as_str());
                if let Some((hir_id, qpath)) = called_fn_path {
                    let typeck_results = tcx.typeck(tcx.hir().local_def_id(dispatch_def_hir_id));
                    if let Some(def_id) = typeck_results.qpath_res(qpath, *hir_id).opt_def_id() {
                        extrinsics_fn_ids.push(def_id);
                    } else {
                        println!("function '{}' not found", match_target.as_str());
                    }
                } else {
                    println!("function '{}' not found", match_target.as_str());
                }
            }
        }
        extrinsics_fn_ids
    }

    fn go_down_dispatch_bypass_filter<'a>(
        current_node: &'a rustc_hir::ExprKind,
        match_target: &'a str,
    ) -> Option<(&'a HirId, &'a rustc_hir::QPath<'a>)> {
        match current_node {
            rustc_hir::ExprKind::Block(block, _) => {
                if let Some(expr) = block.expr {
                    go_down_dispatch_bypass_filter(&expr.kind, match_target)
                } else {
                    None
                }
            }

            rustc_hir::ExprKind::Match(_, arms, _) => {
                for arm in *arms {
                    let rustc_hir::Arm { pat, body, .. } = arm;
                    if is_matching(&pat.kind, match_target) {
                        return go_down_dispatch_bypass_filter(&body.kind, match_target);
                    } else {
                        continue;
                    }
                }
                None
            }

            rustc_hir::ExprKind::MethodCall(_, exprs, _) => match exprs[0].kind {
                rustc_hir::ExprKind::Call(expr, _) => match &expr.kind {
                    rustc_hir::ExprKind::Path(qpath) => Some((&expr.hir_id, qpath)),
                    _ => None,
                },
                _ => go_down_dispatch_bypass_filter(&exprs[0].kind, match_target),
            },
            _ => None,
        }
    }

    fn is_matching(pattern: &rustc_hir::PatKind, match_target: &str) -> bool {
        match pattern {
            rustc_hir::PatKind::Struct(rustc_hir::QPath::TypeRelative(_, path_segment), _, _) => {
                path_segment.ident.as_str() == match_target
            }
            _ => false,
        }
    }
}

pub mod typesystem_storage_variables {
    use super::def_id_printer::*;
    use crate::storage_typesystem::*;
    use crate::typesystem_common::*;
    use rustc_ast::ast::{FloatTy, IntTy, UintTy};
    use rustc_hir::def::Res;
    use rustc_hir::PrimTy;
    use rustc_middle::ty::TyCtxt;

    pub fn get_storage_variables(tcx: &TyCtxt) {
        let mut storage_variables_names = Vec::new();

        for item in tcx.hir().items() {
            let rustc_hir::Item { ident, .. } = item;

            if ident.as_str().contains("_GeneratedPrefixForStorage") {
                storage_variables_names
                    .push(ident.as_str().replace("_GeneratedPrefixForStorage", ""));
            }
        }

        let storage_variables_names = storage_variables_names;

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
                let kind = if let Res::Def(_, def_id) = res {
                    let ident = Ident {
                        name_short: get_def_id_name_with_path(*tcx, *def_id),
                        name_full: get_def_id_name(*tcx, *def_id)
                    };
                    match get_def_id_name_with_path(*tcx, *def_id).as_str() {
                         "frame_support::pallet_prelude::StorageValue" => {
                            if let rustc_hir::GenericArg::Type(ty) = &args[1] {
                                StorageKind::StorageValue {
                                    ident,
                                    value: explore(tcx, ty),
                                }
                             } else {unreachable!()}
                         },
                         "frame_support::pallet_prelude::StorageMap" => {
                            if let rustc_hir::GenericArg::Type(ty) = &args[3] {
                                StorageKind::StorageNMap {
                                    ident,
                                    value: explore(tcx, ty),
                                    //max_values: Some(explore(tcx, &args[6]))
                                }
                             } else {unreachable!()}
                         },
                         "frame_support::pallet_prelude::StorageDoubleMap" => {
                            if let rustc_hir::GenericArg::Type(ty) = &args[3] {
                                StorageKind::StorageNMap {
                                    ident,
                                    value: explore(tcx, ty),
                                    //max_values: Some(explore(tcx, &args[6]))
                                }
                             } else {unreachable!()}

                         },
                         "frame_support::pallet_prelude::StorageNMap" => {
                            if let rustc_hir::GenericArg::Type(ty) = &args[3] {
                                StorageKind::StorageNMap {
                                    ident,
                                    value: explore(tcx, ty),
                                    //max_values: Some(explore(tcx, &args[6]))
                                }
                             } else {unreachable!()}

                         },
                         "frame_support::pallet_prelude::CountedStorageMap" => {
                            if let rustc_hir::GenericArg::Type(ty) = &args[3] {
                                StorageKind::CountedStorageMap {
                                    ident,
                                    value: explore(tcx, ty),
                                    //max_values: Some(explore(tcx, &args[6]))
                                }
                             } else {unreachable!()}

                         },
                         _ => {unreachable!()}
                     }
                } else {
                    unreachable!();
                };

                let storage_type = FrameStorageType::new(tcx, *def_id, kind);
                println!("{:?}, size is {}", storage_type, storage_type.get_size());
            }
        }
    }

    fn explore(tcx: &TyCtxt, ty: &rustc_hir::Ty) -> ValueType {
        if let rustc_hir::Ty {
            kind: rustc_hir::TyKind::Path(qpath),
            ..
        } = ty
        {
            if let rustc_hir::QPath::Resolved(_, path) = qpath {
                return get_value_type(tcx, path);
            } else if let rustc_hir::QPath::TypeRelative(ty, segment) = qpath {
                // Get super type as well
                let super_type = if let rustc_hir::Ty{ kind: rustc_hir::TyKind::Path(qpath), .. } = ty
                && let rustc_hir::QPath::Resolved(_, path) = qpath
                && let rustc_hir::Path { segments, .. } = path {
                    segments[0].ident.as_str()
                } else {
                    unreachable!();
                };

                return ValueType::Symbol(super_type.to_owned() + "::" + segment.ident.as_str());
            }
        } else if let rustc_hir::Ty {
            kind: rustc_hir::TyKind::Tup(tys),
            ..
        } = ty
        {
            let mut members = Vec::new();

            for ty in tys.iter() {
                members.push(Box::new(explore(tcx, ty)));
            }

            return ValueType::Tuple(members);
        }
        unreachable!()
    }

    fn get_value_type(tcx: &TyCtxt, path: &rustc_hir::Path) -> ValueType {
        let rustc_hir::Path { segments, res, .. } = path;

        match res {
            Res::Def(_, def_id) => {
                match get_def_id_name_with_path(*tcx, *def_id).as_str() {
                    "frame_support::BoundedVec" => {
                        let generics = segments[0].args.unwrap().args;
                        if let rustc_hir::GenericArg::Type(ty_0) = &generics[0]
                            && let rustc_hir::GenericArg::Type(ty_1) = &generics[1]
                        {
                            let value = Box::new(explore(tcx, ty_0));
                            let size = match explore(tcx, ty_1) {
                                ValueType::Usize => VecSize::Usize,
                                ValueType::U8 => VecSize::U8,
                                ValueType::U16 => VecSize::U16,
                                ValueType::U32 => VecSize::U32,
                                ValueType::U64 => VecSize::U64,
                                ValueType::U128 => VecSize::U128,
                                ValueType::Symbol(symbol) => VecSize::Symbol(symbol),
                                _ => todo!(),
                            };

                            return ValueType::BoundedVec { value, size };
                        }

                        unreachable!()
                    }
                    "std::option::Option" => {
                        let generic = &segments[0].args.unwrap().args[0];

                        if let rustc_hir::GenericArg::Type(ty) = generic {
                            return ValueType::Option(Box::new(explore(tcx, ty)));
                        }

                        unreachable!()
                    }
                    _ => {
                        // Treat every unkown as symbol, later work will resolve those
                        ValueType::Symbol(get_def_id_name(*tcx, *def_id))
                    }
                }
            }
            Res::PrimTy(prim_ty) => match prim_ty {
                PrimTy::Int(int_ty) => match int_ty {
                    IntTy::Isize => ValueType::Isize,
                    IntTy::I8 => ValueType::I8,
                    IntTy::I16 => ValueType::I16,
                    IntTy::I32 => ValueType::I32,
                    IntTy::I64 => ValueType::I64,
                    IntTy::I128 => ValueType::I128,
                },
                PrimTy::Uint(uint_ty) => match uint_ty {
                    UintTy::Usize => ValueType::Usize,
                    UintTy::U8 => ValueType::U8,
                    UintTy::U16 => ValueType::U16,
                    UintTy::U32 => ValueType::U32,
                    UintTy::U64 => ValueType::U64,
                    UintTy::U128 => ValueType::U128,
                },
                PrimTy::Float(float_ty) => match float_ty {
                    FloatTy::F32 => ValueType::F32,
                    FloatTy::F64 => ValueType::F64,
                },
                PrimTy::Str => ValueType::Str,
                PrimTy::Bool => ValueType::Bool,
                PrimTy::Char => ValueType::Char,
            },
            _ => todo!(),
        }
    }
}
