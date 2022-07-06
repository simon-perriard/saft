use self::{pallet_specs::try_pallet_dispatch, std_specs::try_std_dispatch};

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
        //try_frame_support_dispatch(transfer_function, callee_info)
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
    true
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
        }

        false
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
            return try_std_ops_dispatch(transfer_function, callee_info);
        }

        false
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
                "std::ops::FnOnce::call_once" | "std::ops::Fn::call" | "std::ops::FnMut::call_mut" => {
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
