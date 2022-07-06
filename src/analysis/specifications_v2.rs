use self::{
    frame_support_specs::try_frame_support_dispatch, pallet_specs::try_pallet_dispatch,
    std_specs::try_std_dispatch,
};

use super::cost_analysis::{CalleeInfo, TransferFunction};

pub(crate) fn try_dispatch_to_specifications<'tcx>(
    transfer_function: &mut TransferFunction<'tcx, '_, '_>,
    callee_info: &CalleeInfo<'tcx>,
) -> bool {
    let path = transfer_function
        .tcx
        .def_path_str(callee_info.callee_def_id);
    if path.starts_with("alloc::") {
        //try_alloc_dispatch(transfer_function, callee_info)
    } else if path.starts_with("core::") {
        //try_core_dispatch(transfer_function, callee_info)
    } else if path.starts_with("frame_support::") {
        return try_frame_support_dispatch(transfer_function, callee_info);
    } else if path.starts_with("frame_system::") {
        //try_frame_system_dispatch(transfer_function, callee_info)
    } else if path.starts_with("pallet::Pallet") {
        return try_pallet_dispatch(transfer_function, callee_info);
    } else if path.starts_with("parity_scale_codec::") {
        //try_parity_scale_codec_dispatch(transfer_function, callee_info)
    } else if path.starts_with("sp_io::") {
        //try_sp_io_dispatch(transfer_function, callee_info)
    } else if path.starts_with("sp_runtime::traits::") {
        //try_sp_runtime_traits_dispatch(transfer_function, callee_info);
    } else if path.starts_with("std::") {
        return try_std_dispatch(transfer_function, callee_info);
    } else if path.starts_with("weights::WeightInfo::") {
        // Ignore
    } else {
        //try_custom_dispatch(transfer_function, callee_info)
    }
    false
}

pub(crate) mod frame_support_specs {
    use crate::analysis::cost_analysis::{CalleeInfo, TransferFunction};

    use self::{
        frame_support_pallet_prelude_specs::try_frame_support_pallet_prelude_dispatch,
        frame_support_specs_traits_specs::try_frame_support_traits_dispatch,
    };

    pub(crate) fn try_frame_support_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: &CalleeInfo<'tcx>,
    ) -> bool {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);

        if path.starts_with("frame_support::traits::") {
            try_frame_support_traits_dispatch(transfer_function, callee_info)
        } else if path.starts_with("frame_support::pallet_prelude::") {
            try_frame_support_pallet_prelude_dispatch(transfer_function, callee_info)
        } else {
            false
        }
    }

    mod frame_support_pallet_prelude_specs {
        use std::collections::HashMap;

        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            pallet::Field,
        };

        use rustc_middle::ty::TyKind;
        use rustc_span::def_id::DefId;

        use self::storage_specs::try_storage_dispatch;

        pub(crate) fn try_frame_support_pallet_prelude_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> bool {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);

            if path.starts_with("frame_support::pallet_prelude::Storage") {
                let storage_field = extract_storage_field(transfer_function, callee_info);
                try_storage_dispatch(transfer_function, callee_info, storage_field)
            } else {
                false
            }
        }

        fn extract_storage_field<'tcx>(
            transfer_function: &TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Field {
            let def_id = callee_info.callee_def_id;
            let substs = callee_info.substs_ref;

            let pallet = transfer_function.pallet;
            let tcx = transfer_function.tcx;
            let key = tcx.def_key(def_id);
            let parent_def_id = DefId {
                index: key.parent.unwrap(),
                ..def_id
            };
            let generics = tcx.generics_of(def_id);
            let parent_substs = &substs[..generics.parent_count.min(substs.len())];

            if let TyKind::Adt(adt_def_data, _) = tcx.type_of(parent_def_id).kind() {
                let reconstructed_ty = tcx.mk_adt(*adt_def_data, tcx.intern_substs(parent_substs));

                let ty_field_hash_map = pallet
                    .fields
                    .iter()
                    .map(|(field_def_id, field)| (tcx.type_of(field_def_id), (*field).clone()))
                    .fold(HashMap::new(), |mut accum, (field_ty, field)| {
                        accum.insert(field_ty, field);
                        accum
                    });

                (*ty_field_hash_map.get(&reconstructed_ty).unwrap()).clone()
            } else {
                unreachable!();
            }
        }

        mod storage_specs {
            use crate::analysis::{
                cost_analysis::{CalleeInfo, TransferFunction},
                pallet::Field,
            };

            use self::storage_value_specs::try_storage_value_dispatch;

            pub(crate) fn try_storage_dispatch<'tcx>(
                transfer_function: &mut TransferFunction<'tcx, '_, '_>,
                callee_info: &CalleeInfo<'tcx>,
                storage_field: Field,
            ) -> bool {
                let path = transfer_function
                    .tcx
                    .def_path_str(callee_info.callee_def_id);

                if path.starts_with("frame_support::pallet_prelude::StorageValue::") {
                    try_storage_value_dispatch(transfer_function, callee_info, storage_field)
                } else {
                    false
                }
            }

            mod storage_value_specs {
                use crate::analysis::{
                    cost_analysis::{CalleeInfo, TransferFunction},
                    cost_language::{cost_to_big_o, HasSize},
                    pallet::Field,
                };

                pub(crate) fn try_storage_value_dispatch<'tcx>(
                    transfer_function: &mut TransferFunction<'tcx, '_, '_>,
                    callee_info: &CalleeInfo<'tcx>,
                    storage_field: Field,
                ) -> bool {
                    let path = transfer_function
                        .tcx
                        .def_path_str(callee_info.callee_def_id);
                    let path = path.as_str();

                    match path {
                        "frame_support::pallet_prelude::StorageValue::<Prefix, Value, QueryKind, OnEmpty>::try_mutate" => {
                            // https://paritytech.github.io/substrate/master/frame_support/storage/types/struct.StorageValue.html#method.try_mutate
                            // decoding/encoding depends on the actual length of what is stored

                            transfer_function
                            .state
                            .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));

                            // storage access
                            transfer_function
                                .state
                                .add_reads(storage_field.get_size(transfer_function.tcx));

                            // account for closure complexity
                            todo!();

                            // storage access
                            transfer_function
                                .state
                                .add_writes(storage_field.get_size(transfer_function.tcx));
                            true
                        }
                        _ => false,
                    }
                }
            }
        }
    }

    mod frame_support_specs_traits_specs {
        use crate::analysis::cost_analysis::{CalleeInfo, TransferFunction};

        pub(crate) fn try_frame_support_traits_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> bool {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "frame_support::traits::EnsureOrigin::ensure_origin" => {
                    // https://paritytech.github.io/substrate/master/frame_support/traits/trait.EnsureOrigin.html#method.ensure_origin
                    transfer_function.state.add_step();
                    true
                }
                "frame_support::traits::EnsureOrigin::try_origin" => {
                    transfer_function.state.add_step();
                    true
                }
                "frame_support::traits::Get::get" => {
                    transfer_function.state.add_step();
                    true
                }
                _ => false,
            }
        }
    }
}

pub(crate) mod pallet_specs {
    use regex::Regex;

    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::{Cost, CostParameter},
        events_variants_domain::Variants,
    };
    use rustc_middle::ty::TyKind;

    pub(crate) fn try_pallet_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: &CalleeInfo<'tcx>,
    ) -> bool {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);

        let deposit_event_regex =
            Regex::new(r"pallet::Pallet::<.*\s*(,.*)*>::deposit_event").unwrap();

        if deposit_event_regex.is_match(&path) {
            let args = &callee_info.args;
            let location = callee_info.location;

            let body = transfer_function
                .tcx
                .optimized_mir(transfer_function.def_id);

            if let TyKind::Adt(adt_def, _) = args[0].ty(body, transfer_function.tcx).kind() {
                let event_variants = transfer_function
                    .events_variants
                    .get(&transfer_function.def_id)
                    .unwrap()
                    .get(location.unwrap())
                    .unwrap();

                match event_variants {
                    Variants::Variant(variant_id) => {
                        let ty = args[0].ty(body, transfer_function.tcx);

                        let cost = match transfer_function
                            .tcx
                            .layout_of(transfer_function.tcx.param_env(adt_def.did()).and(ty))
                        {
                            Ok(ty_and_layout) => {
                                let layout = ty_and_layout.layout;
                                match layout.variants() {
                                    rustc_target::abi::Variants::Single { .. } => {
                                        Cost::Scalar(ty_and_layout.layout.size().bytes())
                                    }
                                    rustc_target::abi::Variants::Multiple { variants, .. } => {
                                        let variant_layout = variants[*variant_id];
                                        Cost::Scalar(variant_layout.size().bytes())
                                    }
                                }
                            }
                            Err(_) => {
                                let variant = adt_def.variant(*variant_id);
                                Cost::Parameter(CostParameter::SizeOf(
                                    transfer_function.tcx.def_path_str(variant.def_id),
                                ))
                            }
                        };

                        transfer_function.state.add_events(cost);
                    }
                    Variants::Or(_, _) => {
                        let cost = event_variants
                            .flatten_or()
                            .iter()
                            .map(|variant_id| {
                                let variant = adt_def.variant(*variant_id);
                                // For now add the variant size as symbolic
                                Cost::Parameter(CostParameter::SizeOf(
                                    transfer_function.tcx.def_path_str(variant.def_id),
                                ))
                            })
                            .reduce(|accum, item| accum.max(item))
                            .unwrap();
                        transfer_function.state.add_events(cost);
                    }
                }
            }
            true
        } else {
            false
        }
    }
}

pub(crate) mod std_specs {
    use crate::analysis::cost_analysis::{CalleeInfo, TransferFunction};

    use self::std_ops_specs::try_std_ops_dispatch;

    pub(crate) fn try_std_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: &CalleeInfo<'tcx>,
    ) -> bool {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);

        if path.starts_with("std::ops::") {
            try_std_ops_dispatch(transfer_function, callee_info)
        } else {
            false
        }
    }

    mod std_ops_specs {
        use crate::analysis::cost_analysis::{CalleeInfo, TransferFunction};

        pub(crate) fn try_std_ops_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> bool {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "std::ops::Deref::deref" => {
                    transfer_function.state.add_step();
                    true
                }
                "std::ops::FnOnce::call_once"
                | "std::ops::Fn::call"
                | "std::ops::FnMut::call_mut" => {
                    transfer_function.analyze_closure_call(callee_info);
                    true
                }
                "std::ops::FromResidual::from_residual" => {
                    transfer_function.state.add_step();
                    true
                }
                "std::ops::Try::branch" => {
                    transfer_function.state.add_step();
                    true
                }
                _ => false,
            }
        }
    }
}
