use self::{
    frame_support_specs::try_frame_support_dispatch, pallet_specs::try_pallet_dispatch,
    sp_runtime_specs::try_sp_runtime_dispatch, std_specs::try_std_dispatch,
};

use super::{
    cost_analysis::{CalleeInfo, TransferFunction},
    cost_domain::ExtendedCostAnalysisDomain,
};

pub(crate) fn try_dispatch_to_specifications<'tcx>(
    transfer_function: &mut TransferFunction<'tcx, '_, '_>,
    callee_info: &CalleeInfo<'tcx>,
) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
    let path = transfer_function
        .tcx
        .def_path_str(callee_info.callee_def_id);
    if path.starts_with("alloc::") {
        //try_alloc_dispatch(transfer_function, callee_info)
        None
    } else if path.starts_with("core::") {
        //try_core_dispatch(transfer_function, callee_info)
        None
    } else if path.starts_with("frame_support::") {
        try_frame_support_dispatch(transfer_function, callee_info)
    } else if path.starts_with("frame_system::") {
        //try_frame_system_dispatch(transfer_function, callee_info)
        None
    } else if path.starts_with("pallet::Pallet::") {
        try_pallet_dispatch(transfer_function, callee_info)
    } else if path.starts_with("parity_scale_codec::") {
        //try_parity_scale_codec_dispatch(transfer_function, callee_info)
        None
    } else if path.starts_with("sp_io::") {
        //try_sp_io_dispatch(transfer_function, callee_info)
        None
    } else if path.starts_with("sp_runtime::") {
        try_sp_runtime_dispatch(transfer_function, callee_info)
    } else if path.starts_with("std::") {
        try_std_dispatch(transfer_function, callee_info)
    } else if path.starts_with("weights::WeightInfo::") {
        // Ignore
        Some((*transfer_function.state).clone())
    } else {
        //try_custom_dispatch(transfer_function, callee_info)
        None
    }
}

pub(crate) mod frame_support_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_domain::ExtendedCostAnalysisDomain,
    };

    use self::{
        frame_support_bounded_vec_specs::try_frame_support_bounded_vec,
        frame_support_pallet_prelude_specs::try_frame_support_pallet_prelude_dispatch,
        frame_support_specs_traits_specs::try_frame_support_traits_dispatch,
    };

    pub(crate) fn try_frame_support_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: &CalleeInfo<'tcx>,
    ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);

        if path.starts_with("frame_support::traits::") {
            try_frame_support_traits_dispatch(transfer_function, callee_info)
        } else if path.starts_with("frame_support::pallet_prelude::") {
            try_frame_support_pallet_prelude_dispatch(transfer_function, callee_info)
        } else if path.starts_with("frame_support::") {
            try_frame_support_bounded_vec(transfer_function, callee_info)
        } else {
            None
        }
    }

    mod frame_support_bounded_vec_specs {
        use core::panic;

        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
            cost_language::{Cost, CostParameter},
        };

        pub(crate) fn try_frame_support_bounded_vec<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);

            let path = path.as_str();

            match path {
                "frame_support::BoundedVec::<T, S>::try_push" => {
                    // call "try_push" https://paritytech.github.io/substrate/master/frame_support/storage/bounded_vec/struct.BoundedVec.html#method.try_push
                    // upperbound grow amortized by max size

                    // extract the name of the type for readability
                    let ty_name = callee_info.substs_ref.type_at(1).to_string();
                    let ty_name = ty_name.split("::").last().unwrap();
                    transfer_function
                        .state
                        .add_steps(Cost::BigO(Box::new(Cost::Parameter(
                            CostParameter::ValueOf(format!("{}::get()", ty_name)),
                        ))));
                    Some((*transfer_function.state).clone())
                }
                "frame_support::BoundedVec::<T, S>::get_mut" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "frame_support::BoundedVec::<T, S>::remove" => {
                    // call "remove" https://paritytech.github.io/substrate/master/frame_support/storage/bounded_vec/struct.BoundedVec.html#method.remove
                    // upperbound grow amortized by max size

                    // extract the name of the type for readability
                    let ty_name = callee_info.substs_ref.type_at(1).to_string();
                    let ty_name = ty_name.split("::").last().unwrap();
                    transfer_function
                        .state
                        .add_steps(Cost::BigO(Box::new(Cost::Parameter(
                            CostParameter::ValueOf(format!("{}::get()", ty_name)),
                        ))));
                    Some((*transfer_function.state).clone())
                }
                "frame_support::BoundedVec::<T, S>::retain" => {
                    // call "retain" https://paritytech.github.io/substrate/master/frame_support/storage/bounded_vec/struct.BoundedVec.html#method.retain
                    // extract the name of the type for readability
                    let ty_name = callee_info.substs_ref.type_at(1).to_string();
                    let ty_name = ty_name.split("::").last().unwrap();
                    let length = Cost::BigO(Box::new(Cost::Parameter(CostParameter::ValueOf(
                        format!("{}::get()", ty_name),
                    ))));

                    panic!("{:?}", callee_info.args_type_info);

                    // Retrieve the effect of applying the closure once
                    let closure_call_simulation = CalleeInfo {
                        location: None,
                        args_type_info: todo!(),
                        destination: None,
                        callee_def_id: todo!(),
                        substs_ref: todo!(),
                    };

                    let closure_analysis_result =
                        transfer_function.fn_call_analysis(closure_call_simulation, true);

                    // Multiply the cost by the max size of the BoundedVec
                    closure_analysis_result.cost_big_o_mul(length);

                    transfer_function.state.inter_join(&closure_analysis_result);
                    Some((*transfer_function.state).clone())
                }
                "frame_support::BoundedVec::<T, S>::try_insert" => {
                    // call "try_insert" https://paritytech.github.io/substrate/master/frame_support/storage/bounded_vec/struct.BoundedVec.html#method.try_insert
                    // upperbound grow amortized by max size

                    // extract the name of the type for readability
                    let ty_name = callee_info.substs_ref.type_at(1).to_string();
                    let ty_name = ty_name.split("::").last().unwrap();
                    transfer_function
                        .state
                        .add_steps(Cost::BigO(Box::new(Cost::Parameter(
                            CostParameter::ValueOf(format!("{}::get()", ty_name)),
                        ))));
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }

    mod frame_support_pallet_prelude_specs {
        use std::collections::HashMap;

        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
            pallet::Field,
        };

        use rustc_middle::ty::TyKind;
        use rustc_span::def_id::DefId;

        use self::storage_specs::try_storage_dispatch;

        pub(crate) fn try_frame_support_pallet_prelude_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);

            if path.starts_with("frame_support::pallet_prelude::Storage") {
                let storage_field = extract_storage_field(transfer_function, callee_info);
                try_storage_dispatch(transfer_function, callee_info, storage_field)
            } else {
                None
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
                cost_domain::ExtendedCostAnalysisDomain,
                pallet::Field,
            };

            use self::storage_value_specs::try_storage_value_dispatch;

            pub(crate) fn try_storage_dispatch<'tcx>(
                transfer_function: &mut TransferFunction<'tcx, '_, '_>,
                callee_info: &CalleeInfo<'tcx>,
                storage_field: Field,
            ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
                let path = transfer_function
                    .tcx
                    .def_path_str(callee_info.callee_def_id);

                if path.starts_with("frame_support::pallet_prelude::StorageValue::") {
                    try_storage_value_dispatch(transfer_function, callee_info, storage_field)
                } else {
                    None
                }
            }

            mod storage_value_specs {
                use crate::analysis::{
                    cost_analysis::{CalleeInfo, TransferFunction},
                    cost_domain::ExtendedCostAnalysisDomain,
                    cost_language::{cost_to_big_o, HasSize},
                    pallet::Field,
                };

                use rustc_middle::ty::TyKind;

                pub(crate) fn try_storage_value_dispatch<'tcx>(
                    transfer_function: &mut TransferFunction<'tcx, '_, '_>,
                    callee_info: &CalleeInfo<'tcx>,
                    storage_field: Field,
                ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
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

                            // Account for closure call
                            let closure_adt = callee_info.args_type_info[0].clone();
                            let (closure_fn_ptr, closure_substs_ref) =
                                if let TyKind::Closure(def_id, substs_ref) = closure_adt.get_ty().kind() {
                                    (*def_id, substs_ref)
                                } else if let TyKind::FnDef(def_id, substs_ref) = closure_adt.get_ty().kind() {
                                    (*def_id, substs_ref)
                                } else {
                                    unreachable!();
                                };

                            // Closure accepts only one argument which is of type Value
                            // Rust was able to infer the type in the closure's substs ref
                            // so no need to specialize more the args_type_info
                            let closure_call_simulation = CalleeInfo {
                                location: None,
                                args_type_info: Vec::new(),
                                destination: None,
                                callee_def_id: closure_fn_ptr,
                                substs_ref: closure_substs_ref,
                            };

                            transfer_function.fn_call_analysis(closure_call_simulation, false);

                            // storage access
                            transfer_function
                                .state
                                .add_writes(storage_field.get_size(transfer_function.tcx));
                            Some((*transfer_function.state).clone())
                        }
                        _ => None,
                    }
                }
            }
        }
    }

    mod frame_support_specs_traits_specs {
        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
        };

        pub(crate) fn try_frame_support_traits_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "frame_support::traits::EnsureOrigin::ensure_origin" => {
                    // https://paritytech.github.io/substrate/master/frame_support/traits/trait.EnsureOrigin.html#method.ensure_origin
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "frame_support::traits::EnsureOrigin::try_origin" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "frame_support::traits::Get::get" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }
}

pub(crate) mod pallet_specs {
    use regex::Regex;

    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_domain::ExtendedCostAnalysisDomain,
        cost_language::{Cost, CostParameter},
        events_variants_domain::Variants,
    };
    use rustc_middle::ty::TyKind;

    pub(crate) fn try_pallet_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: &CalleeInfo<'tcx>,
    ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);

        let deposit_event_regex =
            Regex::new(r"pallet::Pallet::<.*\s*(,.*)*>::deposit_event").unwrap();

        if deposit_event_regex.is_match(&path) {
            let args = &callee_info.args_type_info;
            let location = callee_info.location;

            if let TyKind::Adt(adt_def, _) = args[0].get_ty().kind() {
                let event_variants = transfer_function
                    .events_variants
                    .get(&transfer_function.def_id)
                    .unwrap()
                    .get(location.unwrap())
                    .unwrap();

                match event_variants {
                    Variants::Variant(variant_id) => {
                        let ty = args[0].get_ty();

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
            Some((*transfer_function.state).clone())
        } else {
            None
        }
    }
}

pub(crate) mod sp_runtime_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_domain::ExtendedCostAnalysisDomain,
    };

    use self::sp_runtime_traits_specs::try_sp_runtime_traits_dispatch;

    pub(crate) fn try_sp_runtime_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: &CalleeInfo<'tcx>,
    ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);

        if path.starts_with("sp_runtime::traits::") {
            try_sp_runtime_traits_dispatch(transfer_function, callee_info)
        } else {
            None
        }
    }

    mod sp_runtime_traits_specs {
        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
        };

        pub(crate) fn try_sp_runtime_traits_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);

            let path = path.as_str();

            match path {
                "sp_runtime::traits::Zero::zero" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "sp_runtime::traits::Zero::is_zero" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "sp_runtime::traits::One::one" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }
}

pub(crate) mod std_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_domain::ExtendedCostAnalysisDomain,
    };

    use self::{std_default_specs::try_std_default_dispatch, std_ops_specs::try_std_ops_dispatch, std_convert_specs::try_std_convert_dispatch};

    pub(crate) fn try_std_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: &CalleeInfo<'tcx>,
    ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);

        if path.starts_with("std::default::") {
            try_std_default_dispatch(transfer_function, callee_info)
        } else if path.starts_with("std::ops::") {
            try_std_ops_dispatch(transfer_function, callee_info)
        } else if path.starts_with("std::convert::") {
            try_std_convert_dispatch(transfer_function, callee_info)
        } else {
            None
        }
    }

    mod std_convert_specs {
        use crate::analysis::{cost_analysis::{TransferFunction, CalleeInfo}, cost_domain::ExtendedCostAnalysisDomain};

        pub(crate) fn try_std_convert_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "std::convert::Into::into" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::convert::From::from" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::convert::AsRef::as_ref" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }

    mod std_default_specs {
        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
        };

        pub(crate) fn try_std_default_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "std::default::Default::default" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }

    mod std_ops_specs {
        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
        };

        pub(crate) fn try_std_ops_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "std::ops::Deref::deref" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::ops::FnOnce::call_once"
                | "std::ops::Fn::call"
                | "std::ops::FnMut::call_mut" => {
                    transfer_function.analyze_closure_call(callee_info);
                    Some((*transfer_function.state).clone())
                }
                "std::ops::FromResidual::from_residual" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::ops::Try::branch" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }
}
