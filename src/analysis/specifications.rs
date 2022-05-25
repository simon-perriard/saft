use crate::analysis::specifications::std_ops_specs::std_ops_dispatch;

use crate::analysis::cost_analysis::{CalleeInfo, TransferFunction};
use rustc_middle::ty::TyCtxt;

use self::frame_support_traits_specs::frame_support_traits_dispatch;
use self::sp_runtime_traits_specs::sp_runtime_traits_dispatch;
use self::std_convert_specs::std_convert_dispatch;
use self::std_default_specs::std_default_dispatch;

pub(crate) fn dispatch_to_specifications<'tcx>(
    tcx: TyCtxt<'tcx>,
    transfer_function: &mut TransferFunction,
    callee_info: CalleeInfo<'tcx>,
) {
    let path = tcx.def_path_str(callee_info.callee_def_id);

    if path.starts_with("std::ops::") {
        std_ops_dispatch(tcx, transfer_function, callee_info);
    } else if path.starts_with("std::convert::") {
        std_convert_dispatch(tcx, transfer_function, callee_info);
    } else if path.starts_with("std::default::") {
        std_default_dispatch(tcx, transfer_function, callee_info);
    } else if path.starts_with("frame_support::traits::") {
        frame_support_traits_dispatch(tcx, transfer_function, callee_info);
    } else if path.starts_with("sp_runtime::traits::") {
        sp_runtime_traits_dispatch(tcx, transfer_function, callee_info);
    } else if path.starts_with("weights::WeightInfo::") {
        // Ignore
    } else {
        println!("{} --- {:?}", path, callee_info.func);
    }
}

pub(crate) mod std_ops_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::{Cost, Symbolic},
    };
    use rustc_middle::ty::TyCtxt;

    pub(crate) fn std_ops_dispatch<'tcx>(
        tcx: TyCtxt<'tcx>,
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
    ) {
        let path = tcx.def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "std::ops::Try::branch" => {
                let ty_path = callee_info
                    .substs_ref
                    .type_at(0)
                    .sort_string(tcx)
                    .into_owned();
                match ty_path.as_str() {
                    "enum `std::result::Result`" => {
                        /* Account for function call /!\ INLINED                                        https://doc.rust-lang.org/nightly/std/result/enum.Result.html#method.branch
                            -> no cost
                        **/
                    }
                    _ => {
                        let ty_path = callee_info.substs_ref.type_at(0).to_string();
                        let normalized_fn_path = ty_path + "::branch()";
                        transfer_function
                            .domain_state
                            .add_steps(Cost::Symbolic(Symbolic::TimeOf(normalized_fn_path)));
                    }
                }
            }
            "std::ops::FromResidual::from_residual" => {
                let ty_path = callee_info.substs_ref.type_at(0).to_string();
                let ty_path = ty_path.as_str();
                match ty_path {
                    "std::result::Result<frame_support::dispatch::PostDispatchInfo, sp_runtime::DispatchErrorWithPostInfo<frame_support::dispatch::PostDispatchInfo>>"
                    if vec!["std::result::Result<std::convert::Infallible, frame_support::error::BadOrigin>", "std::result::Result<std::convert::Infallible, sp_runtime::DispatchError>"]
                        .contains(&callee_info.substs_ref.type_at(1).to_string().as_str())  => {

                        if callee_info.substs_ref.type_at(1).to_string().as_str() == "std::result::Result<std::convert::Infallible, frame_support::error::BadOrigin>" {
                            /* Account for function call /!\ INLINED +                                  https://doc.rust-lang.org/nightly/std/result/enum.Result.html#method.from_residual-2
                                DispatchErrorWithPostInfo::from(BadOrigin as Into<DispatchError>) +     https://docs.substrate.io/rustdocs/latest/sp_runtime/struct.DispatchErrorWithPostInfo.html#method.from
                                    BadOrigin::into(DispatchError) +                                    https://docs.substrate.io/rustdocs/latest/sp_runtime/traits/struct.BadOrigin.html#method.into
                                        DispatchError::From(BadOrigin)                                  https://docs.substrate.io/rustdocs/latest/sp_runtime/enum.DispatchError.html#method.from-2
                            */
                            transfer_function.domain_state.add_steps(Cost::Concrete(3));
                        } else {
                            /* Account for function call /!\ INLINED +                                  https://doc.rust-lang.org/nightly/std/result/enum.Result.html#method.from_residual-2
                                DispatchErrorWithPostInfo::from(DispatchErroras Into<DispatchError>) +  https://docs.substrate.io/rustdocs/latest/sp_runtime/struct.DispatchErrorWithPostInfo.html#method.from
                                    DispatchError::into(DispatchError) +                                https://docs.substrate.io/rustdocs/latest/frame_support/dispatch/enum.DispatchError.html#method.into
                                        DispatchError::From(DispatchError)                              https://docs.substrate.io/rustdocs/latest/frame_support/dispatch/enum.DispatchError.html#method.from-7
                            */
                            transfer_function.domain_state.add_steps(Cost::Concrete(3));
                        }
                    },
                    _ => {
                        let ty_path = callee_info.substs_ref.type_at(0).to_string();
                        let arg_ty_path = callee_info.substs_ref.type_at(1).to_string();
                        let normalized_fn_path = format!("{}::from_residual({})", ty_path, arg_ty_path);
                        transfer_function
                            .domain_state
                            .add_steps(Cost::Symbolic(Symbolic::TimeOf(normalized_fn_path)));
                    }
                }
            }
            "std::ops::Deref::deref" => {
                let ty_path = callee_info
                    .substs_ref
                    .type_at(0)
                    .sort_string(tcx)
                    .into_owned();

                match ty_path.as_str() {
                    "struct `frame_support::BoundedVec`" => {
                        // Account for function call                                                    https://docs.substrate.io/rustdocs/latest/frame_support/storage/bounded_vec/struct.BoundedVec.html#method.deref
                        transfer_function.domain_state.add_steps(Cost::Concrete(1));
                    }
                    _ => {
                        let ty_path = callee_info.substs_ref.type_at(0).to_string();
                        let normalized_fn_path = ty_path + "::deref()";
                        transfer_function
                            .domain_state
                            .add_steps(Cost::Symbolic(Symbolic::TimeOf(normalized_fn_path)));
                    }
                }
            }
            _ => {
                unimplemented!("{} --- {:?}", path, callee_info.func);
            }
        }
    }
}

pub(crate) mod std_convert_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::{Cost, Symbolic},
    };
    use rustc_middle::ty::TyCtxt;

    pub(crate) fn std_convert_dispatch(
        tcx: TyCtxt,
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo,
    ) {
        let path = tcx.def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "std::convert::Into::into" => {
                let ty_path_from = callee_info.substs_ref.type_at(0).to_string();
                let ty_path_from = ty_path_from.as_str();

                let ty_path_into = callee_info.substs_ref.type_at(1).to_string();
                let ty_path_into = ty_path_into.as_str();

                match (ty_path_from, ty_path_into) {
                    ("std::option::Option<u64>", "frame_support::dispatch::PostDispatchInfo") => {
                        /* Account for function call +                                              https://doc.rust-lang.org/std/option/enum.Option.html#method.into
                            PostDispatchInfo::from(Option<u64>)                                     https://docs.substrate.io/rustdocs/latest/frame_support/weights/struct.PostDispatchInfo.html#method.from-2
                                Pays::default                                                       https://docs.substrate.io/rustdocs/latest/frame_support/weights/enum.Pays.html#method.default
                        **/
                        transfer_function.domain_state.add_steps(Cost::Concrete(3));
                    }
                    _ => {
                        let normalized_fn_path =
                            format!("{}::into({})", ty_path_from, ty_path_into);
                        transfer_function
                            .domain_state
                            .add_steps(Cost::Symbolic(Symbolic::TimeOf(normalized_fn_path)));
                    }
                }
            }
            _ => {
                unimplemented!("{} --- {:?}", path, callee_info.func);
            }
        }
    }
}

pub(crate) mod std_default_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::{Cost, Symbolic},
    };
    use rustc_middle::ty::TyCtxt;

    pub(crate) fn std_default_dispatch<'tcx>(
        tcx: TyCtxt<'tcx>,
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
    ) {
        let path = tcx.def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "std::default::Default::default" => {
                let ty_path = callee_info.substs_ref.type_at(0).to_string();

                match ty_path.as_str() {
                    "types::IdentityFields" => {
                        /* Account for function call /!\ INLINED +                                  https://docs.substrate.io/rustdocs/latest/pallet_identity/struct.IdentityFields.html#method.default
                            BitFlags::default /!\ INLINED                                           https://docs.rs/enumflags2/latest/enumflags2/struct.BitFlags.html#method.default
                            -> no cost
                        **/
                    }
                    _ => {
                        let ty_path = callee_info.substs_ref.type_at(0).to_string();
                        let normalized_fn_path = ty_path + "::default()";
                        transfer_function
                            .domain_state
                            .add_steps(Cost::Symbolic(Symbolic::TimeOf(normalized_fn_path)));
                    }
                }
            }
            _ => {
                unimplemented!("{} --- {:?}", path, callee_info.func);
            }
        }
    }
}

pub(crate) mod frame_support_traits_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::{Cost, Symbolic},
    };
    use rustc_middle::ty::TyCtxt;

    pub(crate) fn frame_support_traits_dispatch<'tcx>(
        tcx: TyCtxt<'tcx>,
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
    ) {
        let path = tcx.def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "frame_support::traits::EnsureOrigin::ensure_origin" => {
                /* Account for function call +                                                         https://docs.substrate.io/rustdocs/latest/frame_support/traits/trait.EnsureOrigin.html#method.ensure_origin
                    try_origin
                */
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
                account_for_try_origin(transfer_function, callee_info);
            }
            "frame_support::traits::EnsureOrigin::try_origin" => {
                account_for_try_origin(transfer_function, callee_info);
            }
            _ => {
                unimplemented!("{} --- {:?}", path, callee_info.func);
            }
        }
    }

    fn account_for_try_origin<'tcx>(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
    ) {
        let ty_high_order = callee_info.substs_ref.type_at(0).to_string();
        let ty_arg = transfer_function.local_types.borrow()
            [callee_info.args[0].place().unwrap().local]
            .to_string();

        transfer_function
            .domain_state
            .add_steps(Cost::Symbolic(Symbolic::TimeOf(format!(
                "{}::try_origin({})",
                ty_high_order, ty_arg
            ))));
    }
}

pub(crate) mod sp_runtime_traits_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::{Cost, Symbolic},
    };
    use rustc_middle::ty::TyCtxt;

    pub(crate) fn sp_runtime_traits_dispatch<'tcx>(
        tcx: TyCtxt<'tcx>,
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
    ) {
        let path = tcx.def_path_str(callee_info.callee_def_id);
        let path = path.as_str();

        match path {
            "sp_runtime::traits::Zero::zero" => {
                let ty_path = callee_info.substs_ref.type_at(0).to_string();
                match ty_path.as_str() {
                    _ => {
                        let ty_path = callee_info.substs_ref.type_at(0).to_string();
                        let normalized_fn_path = ty_path + "::zero()";
                        transfer_function
                            .domain_state
                            .add_steps(Cost::Symbolic(Symbolic::TimeOf(normalized_fn_path)));
                    }
                }
            }
            _ => {
                unimplemented!("{} --- {:?}", path, callee_info.func);
            }
        }
    }
}

pub(crate) mod storage_actions_specs {

    use crate::analysis::{
        cost_language::{Cost, Symbolic, HasSize},
        pallet::{Field, StorageKind},
    };
    use rustc_middle::ty::{TyCtxt, TyKind};

    // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html
    // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageMap.html

    #[derive(Debug)]
    pub(crate) struct AccessCost {
        pub reads: Cost,
        pub writes: Cost,
        pub steps: Cost,
    }

    impl AccessCost {
        pub fn new(reads: Cost, writes: Cost, steps: Cost) -> Self {
            AccessCost {
                reads,
                writes,
                steps,
            }
        }
    }

    pub(crate) trait HasAccessCost {
        fn get_access_cost(&self, tcx: TyCtxt, action: &str) -> Option<AccessCost>;
    }

    impl HasAccessCost for Field {
        fn get_access_cost(&self, tcx: TyCtxt, action: &str) -> Option<AccessCost> {
            match &self.kind {
                StorageKind::StorageValue { .. } => {
                    StorageValueActions::get_access_cost(tcx, &self, action)
                }
                StorageKind::StorageMap { .. } => {
                    StorageMapActions::get_access_cost(tcx, &self, action)
                }
                StorageKind::StorageDoubleMap { .. } => {
                    StorageDoubleMapActions::get_access_cost(tcx, &self, action)
                }
                StorageKind::StorageNMap { .. } => todo!(),
                StorageKind::CountedStorageMap { .. } => todo!(),
            }
        }
    }

    pub(crate) enum StorageValueActions {
        Append,
        DecodeLen,
        Exists,
        Get,
        Kill,
        Mutate,
        Put,
        Set,
        Take,
        Translate,
        TryAppend,
        TryGet,
        TryMutate,
    }

    impl StorageValueActions {
        fn is_storage_value_action(action: &str) -> bool {
            StorageValueActions::storage_value_actions().contains(&action.to_owned())
        }

        fn storage_value_actions() -> Vec<String> {
            let storage_value_actions = vec![
                "append",
                "decode_len",
                "exists",
                "get",
                "kill",
                "mutate",
                "put",
                "set",
                "take",
                "translate",
                "try_append",
                "try_get",
                "try_mutate",
            ];

            storage_value_actions
                .iter()
                .map(|action| String::from(*action))
                .collect()
        }

        fn from(action: &str) -> StorageValueActions {
            match action {
                "append" => StorageValueActions::Append,
                "decode_len" => StorageValueActions::DecodeLen,
                "exists" => StorageValueActions::Exists,
                "get" => StorageValueActions::Get,
                "kill" => StorageValueActions::Kill,
                "mutate" => StorageValueActions::Mutate,
                "put" => StorageValueActions::Put,
                "set" => StorageValueActions::Set,
                "take" => StorageValueActions::Take,
                "translate" => StorageValueActions::Translate,
                "try_append" => StorageValueActions::TryAppend,
                "try_get" => StorageValueActions::TryGet,
                "try_mutate" => StorageValueActions::TryMutate,
                _ => panic!("Invalid StorageValue action"),
            }
        }

        pub fn get_access_cost(tcx: TyCtxt, field: &Field, action: &str) -> Option<AccessCost> {
            let action_short = action.split("::").last().unwrap();

            if !Self::is_storage_value_action(action_short) {
                None
            } else {

                let substs_ref = if let TyKind::Adt(_, substs_ref) = tcx.type_of(field.def_id).kind() {
                    substs_ref
                } else {
                    unreachable!()
                };

                let mut steps = Cost::default();
                let mut reads = Cost::default();
                let mut writes = Cost::default();

                match Self::from(action_short) {
                    StorageValueActions::Append => todo!(),
                    StorageValueActions::DecodeLen => todo!(),
                    StorageValueActions::Exists => todo!(),
                    StorageValueActions::Get => {
                            steps = steps + Cost::Concrete(1);
                            // compute storage key
                                steps = steps + Cost::Symbolic(Symbolic::TimeOf(format!("{}::storage_value_final_key()", tcx.def_path_str(field.def_id))));
                                // get data from storage
                                reads = reads + field.get_size(&tcx);
                                // decode the whole data
                                steps = steps + Cost::Symbolic(Symbolic::TimeOf(format!("decode({:?})", substs_ref[1])));
                                // from_optional_to_query_value
                                steps = steps + Cost::Concrete(1);
                    },
                    StorageValueActions::Kill => {
                        // call "kill"
                        steps = steps + Cost::Concrete(1);
                        // compute storage key
                        steps = steps + Cost::Symbolic(Symbolic::TimeOf(format!("{}::storage_value_final_key()", tcx.def_path_str(field.def_id))));
                        steps = steps + Cost::Symbolic(Symbolic::TimeOf(format!("unhashed::kill()",)));
                        // Write None to database
                        writes = writes + Cost::Concrete(1);

                    },
                    StorageValueActions::Mutate => todo!(),
                    StorageValueActions::Put => {
                        // call "put"
                        steps = steps + Cost::Concrete(1);
                            // compute storage key
                            steps = steps + Cost::Symbolic(Symbolic::TimeOf(format!("{}::storage_value_final_key()", tcx.def_path_str(field.def_id))));
                            // call unhashed::put
                            steps = steps + Cost::Symbolic(Symbolic::TimeOf(format!("unhashed::put()",)));
                            writes = writes + field.get_size(&tcx);
                    },
                    StorageValueActions::Set => todo!(),
                    StorageValueActions::Take => todo!(),
                    StorageValueActions::Translate => todo!(),
                    StorageValueActions::TryAppend => todo!(),
                    StorageValueActions::TryGet => todo!(),
                    StorageValueActions::TryMutate => {
                        // call try_mutate https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.try_mutate
                        steps = steps + Cost::Concrete(1);
                            // call get https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/value.rs#63
                            let get_access_cost = Self::get_access_cost(tcx, field, "get")?;
                            reads = reads + get_access_cost.reads;
                            writes = writes + get_access_cost.writes;
                            steps = steps + get_access_cost.steps;
                            // apply closure (done in analyze_storage_access)
                            // "if" branch is more costly, account for call to is_ok()
                            steps = steps + Cost::Concrete(1);
                            // call from_query_to_optional_value
                            steps = steps + Cost::Concrete(1);

                            // "put" has greater cost than "kill"
                            let put_access_cost = Self::get_access_cost(tcx, field, "put")?;
                            reads = reads + put_access_cost.reads;
                            writes = writes + put_access_cost.writes;
                            steps = steps + put_access_cost.steps;

                    },
                };
                Some(AccessCost::new(reads, writes, steps))

            }
        }
    }

    pub(crate) enum StorageMapActions {
        Append,
        ContainsKey,
        DecodeLen,
        Drain,
        Get,
        Insert,
        Iter,
        IterFrom,
        IterKeys,
        IterKeysFrom,
        IterValues,
        MigrateKey,
        Mutate,
        MutateExists,
        Remove,
        RemoveAll,
        Swap,
        Take,
        Translate,
        TranslateValues,
        TryAppend,
        TryGet,
        TryMutate,
        TryMutateExists,
    }

    impl StorageMapActions {
        fn is_storage_map_action(action: &str) -> bool {
            StorageMapActions::storage_map_actions().contains(&action.to_owned())
        }

        fn storage_map_actions() -> Vec<String> {
            let storage_map_actions = vec![
                "append",
                "contains_key",
                "decode_len",
                "drain",
                "get",
                "insert",
                "iter",
                "iter_from",
                "iter_keys",
                "iter_keys_from",
                "iter_values",
                "migrate_key",
                "mutate",
                "mutate_exists",
                "remove",
                "remove_all",
                "swap",
                "take",
                "translate",
                "translate_values",
                "try_append",
                "try_get",
                "try_mutate",
                "try_mutate_exists",
            ];

            storage_map_actions
                .iter()
                .map(|action| String::from(*action))
                .collect()
        }

        fn from(action: &str) -> StorageMapActions {
            match action {
                "append" => StorageMapActions::Append,
                "contains_key" => StorageMapActions::ContainsKey,
                "decode_len" => StorageMapActions::DecodeLen,
                "drain" => StorageMapActions::Drain,
                "get" => StorageMapActions::Get,
                "insert" => StorageMapActions::Insert,
                "iter" => StorageMapActions::Iter,
                "iter_from" => StorageMapActions::IterFrom,
                "iter_keys" => StorageMapActions::IterKeys,
                "iter_keys_from" => StorageMapActions::IterKeysFrom,
                "iter_values" => StorageMapActions::IterValues,
                "migrate_key" => StorageMapActions::MigrateKey,
                "mutate" => StorageMapActions::Mutate,
                "mutate_exists" => StorageMapActions::MutateExists,
                "remove" => StorageMapActions::Remove,
                "remove_all" => StorageMapActions::RemoveAll,
                "swap" => StorageMapActions::Swap,
                "take" => StorageMapActions::Take,
                "translate" => StorageMapActions::Translate,
                "translate_values" => StorageMapActions::TranslateValues,
                "try_append" => StorageMapActions::TryAppend,
                "try_get" => StorageMapActions::TryGet,
                "try_mutate" => StorageMapActions::TryMutate,
                "try_mutate_exists" => StorageMapActions::TryMutateExists,
                _ => panic!("Invalid StorageMap action"),
            }
        }

        pub fn get_access_cost(tcx: TyCtxt, field: &Field, action: &str) -> Option<AccessCost> {
            let action_short = action.split("::").last().unwrap();

            if !Self::is_storage_map_action(action_short) {
                None
            } else {

                let substs_ref = if let TyKind::Adt(_, substs_ref) = tcx.type_of(field.def_id).kind() {
                    substs_ref
                } else {
                    unreachable!()
                };

                let mut steps = Cost::default();
                let mut reads = Cost::default();
                let mut writes = Cost::default();

                match Self::from(action_short) {
                    StorageMapActions::Append => todo!(),
                    StorageMapActions::ContainsKey => todo!(),
                    StorageMapActions::DecodeLen => todo!(),
                    StorageMapActions::Drain => todo!(),
                    StorageMapActions::Get => todo!(),
                    StorageMapActions::Insert => todo!(),
                    StorageMapActions::Iter => todo!(),
                    StorageMapActions::IterFrom => todo!(),
                    StorageMapActions::IterKeys => todo!(),
                    StorageMapActions::IterKeysFrom => todo!(),
                    StorageMapActions::IterValues => todo!(),
                    StorageMapActions::MigrateKey => todo!(),
                    StorageMapActions::Mutate => todo!(),
                    StorageMapActions::MutateExists => todo!(),
                    StorageMapActions::Remove => todo!(),
                    StorageMapActions::RemoveAll => todo!(),
                    StorageMapActions::Swap => todo!(),
                    StorageMapActions::Take => todo!(),
                    StorageMapActions::Translate => todo!(),
                    StorageMapActions::TranslateValues => todo!(),
                    StorageMapActions::TryAppend => todo!(),
                    StorageMapActions::TryGet => todo!(),
                    StorageMapActions::TryMutate => todo!(),
                    StorageMapActions::TryMutateExists => todo!(),
                };
                Some(AccessCost::new(reads, writes, steps))
            }
        }
    }

    pub(crate) enum StorageDoubleMapActions {
        Append,
        ContainsKey,
        DecodeLen,
        Drain,
        DrainPrefix,
        Get,
        Insert,
        Iter,
        IterFrom,
        IterKeyPrefix,
        IterKeyPrefixFrom,
        IterKeys,
        IterKeysFrom,
        IterPrefix,
        IterPrefixFrom,
        IterPrefixValues,
        IterValues,
        MigrateKeys,
        Mutate,
        MutateExists,
        Remove,
        RemoveAll,
        RemovePrefix,
        Swap,
        Take,
        Translate,
        TranslateValues,
        TryAppend,
        TryGet,
        TryMutate,
        TryMutateExists,
    }

    impl StorageDoubleMapActions {
        fn is_storage_map_action(action: &str) -> bool {
            StorageDoubleMapActions::storage_double_map_actions().contains(&action.to_owned())
        }

        fn storage_double_map_actions() -> Vec<String> {
            let storage_double_map_actions = vec![
                "append",
                "contains_key",
                "decode_len",
                "drain",
                "drain_prefix",
                "get",
                "insert",
                "iter",
                "iter_from",
                "iter_key_prefix",
                "iter_key_prefix_from",
                "iter_keys",
                "iter_keys_from",
                "iter_prefix",
                "iter_prefix_from",
                "iter_prefix_values",
                "iter_values",
                "migrate_keys",
                "mutate",
                "mutate_exists",
                "remove",
                "remove_all",
                "remove_prefix",
                "swap",
                "take",
                "translate",
                "translate_values",
                "try_append",
                "try_get",
                "try_mutate",
                "try_mutate_exists",
            ];

            storage_double_map_actions
                .iter()
                .map(|action| String::from(*action))
                .collect()
        }

        fn from(action: &str) -> StorageDoubleMapActions {
            match action {
                "append" => StorageDoubleMapActions::Append,
                "contains_key" => StorageDoubleMapActions::ContainsKey,
                "decode_len" => StorageDoubleMapActions::DecodeLen,
                "drain" => StorageDoubleMapActions::Drain,
                "drain_prefix" => StorageDoubleMapActions::DrainPrefix,
                "get" => StorageDoubleMapActions::Get,
                "insert" => StorageDoubleMapActions::Insert,
                "iter" => StorageDoubleMapActions::Iter,
                "iter_from" => StorageDoubleMapActions::IterFrom,
                "iter_key_prefix" => StorageDoubleMapActions::IterKeyPrefix,
                "iter_key_prefix_from" => StorageDoubleMapActions::IterKeyPrefixFrom,
                "iter_keys" => StorageDoubleMapActions::IterKeys,
                "iter_keys_from" => StorageDoubleMapActions::IterKeysFrom,
                "iter_prefix" => StorageDoubleMapActions::IterPrefix,
                "iter_prefix_from" => StorageDoubleMapActions::IterPrefixFrom,
                "iter_prefix_values" => StorageDoubleMapActions::IterPrefixValues,
                "iter_values" => StorageDoubleMapActions::IterValues,
                "migrate_keys" => StorageDoubleMapActions::MigrateKeys,
                "mutate" => StorageDoubleMapActions::Mutate,
                "mutate_exists" => StorageDoubleMapActions::MutateExists,
                "remove" => StorageDoubleMapActions::Remove,
                "remove_all" => StorageDoubleMapActions::RemoveAll,
                "remove_prefix" => StorageDoubleMapActions::RemovePrefix,
                "swap" => StorageDoubleMapActions::Swap,
                "take" => StorageDoubleMapActions::Take,
                "translate" => StorageDoubleMapActions::Translate,
                "translate_values" => StorageDoubleMapActions::TranslateValues,
                "try_append" => StorageDoubleMapActions::TryAppend,
                "try_get" => StorageDoubleMapActions::TryGet,
                "try_mutate" => StorageDoubleMapActions::TryMutate,
                "try_mutate_exists" => StorageDoubleMapActions::TryMutateExists,
                _ => panic!("Invalid StorageDoubleMap action"),
            }
        }

        pub fn get_access_cost(tcx: TyCtxt, field: &Field, action: &str) -> Option<AccessCost> {
            let action_short = action.split("::").last().unwrap();

            if !Self::is_storage_map_action(action_short) {
                None
            } else {

                let substs_ref = if let TyKind::Adt(_, substs_ref) = tcx.type_of(field.def_id).kind() {
                    substs_ref
                } else {
                    unreachable!()
                };

                let mut steps = Cost::default();
                let mut reads = Cost::default();
                let mut writes = Cost::default();

                match Self::from(action_short) {
                    StorageDoubleMapActions::Append => todo!(),
                    StorageDoubleMapActions::ContainsKey => todo!(),
                    StorageDoubleMapActions::DecodeLen => todo!(),
                    StorageDoubleMapActions::Drain => todo!(),
                    StorageDoubleMapActions::DrainPrefix => todo!(),
                    StorageDoubleMapActions::Get => todo!(),
                    StorageDoubleMapActions::Insert => todo!(),
                    StorageDoubleMapActions::Iter => todo!(),
                    StorageDoubleMapActions::IterFrom => todo!(),
                    StorageDoubleMapActions::IterKeyPrefix => todo!(),
                    StorageDoubleMapActions::IterKeyPrefixFrom => todo!(),
                    StorageDoubleMapActions::IterKeys => todo!(),
                    StorageDoubleMapActions::IterKeysFrom => todo!(),
                    StorageDoubleMapActions::IterPrefix => todo!(),
                    StorageDoubleMapActions::IterPrefixFrom => todo!(),
                    StorageDoubleMapActions::IterPrefixValues => todo!(),
                    StorageDoubleMapActions::IterValues => todo!(),
                    StorageDoubleMapActions::MigrateKeys => todo!(),
                    StorageDoubleMapActions::Mutate => todo!(),
                    StorageDoubleMapActions::MutateExists => todo!(),
                    StorageDoubleMapActions::Remove => todo!(),
                    StorageDoubleMapActions::RemoveAll => todo!(),
                    StorageDoubleMapActions::RemovePrefix => todo!(),
                    StorageDoubleMapActions::Swap => todo!(),
                    StorageDoubleMapActions::Take => todo!(),
                    StorageDoubleMapActions::Translate => todo!(),
                    StorageDoubleMapActions::TranslateValues => todo!(),
                    StorageDoubleMapActions::TryAppend => todo!(),
                    StorageDoubleMapActions::TryGet => todo!(),
                    StorageDoubleMapActions::TryMutate => todo!(),
                    StorageDoubleMapActions::TryMutateExists => todo!(),
                };
                Some(AccessCost::new(reads, writes, steps))
            }
        }
    }
}
