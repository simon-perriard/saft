use crate::analysis::cost_analysis::{CalleeInfo, TransferFunction};

use self::frame_support_specs::frame_support_dispatch;
use self::frame_system_specs::frame_system_dispatch;
use self::parity_scale_codec_specs::parity_scale_codec_dispatch;
use self::sp_io_specs::sp_io_dispatch;
use self::sp_runtime_traits_specs::sp_runtime_traits_dispatch;
use self::std_specs::std_dispatch;

use super::cost_analysis::SummaryKey;

pub(crate) fn needs_early_catch(path: &str) -> bool {
    match path {
        "std::slice::<impl [T]>::to_vec" => true,
        _ => false
    }
}

pub(crate) fn dispatch_to_specifications<'tcx>(
    transfer_function: &mut TransferFunction<'tcx, '_, '_>,
    callee_info: CalleeInfo<'tcx>,
    args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
) {
    let path = transfer_function
        .tcx
        .def_path_str(callee_info.callee_def_id);

    if path.starts_with("frame_support::") {
        frame_support_dispatch(transfer_function, callee_info, args_summary_keys);
    } else if path.starts_with("frame_system::") {
        frame_system_dispatch(transfer_function, callee_info, args_summary_keys);
    } else if path.starts_with("parity_scale_codec::") {
        parity_scale_codec_dispatch(transfer_function, callee_info, args_summary_keys);
    } else if path.starts_with("sp_io::") {
        sp_io_dispatch(transfer_function, callee_info, args_summary_keys);
    } else if path.starts_with("sp_runtime::traits::") {
        sp_runtime_traits_dispatch(transfer_function, callee_info, args_summary_keys);
    } else if path.starts_with("std::") {
        std_dispatch(transfer_function, callee_info, args_summary_keys);
    } else if path.starts_with("weights::WeightInfo::") {
        // Ignore
    } else {
        unimplemented!(
            "{} --- {:?}",
            path,
            transfer_function
                .tcx
                .mk_fn_def(callee_info.callee_def_id, callee_info.substs_ref)
        );
    }
}

pub(crate) mod frame_support_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::{get_big_o_from_storage_size, HasSize},
        types::Type,
    };

    use super::{
        frame_support_bounded_vec_specs::frame_support_bounded_vec_dispatch,
        frame_support_dispatch_specs::frame_support_dispatch_dispatch,
        frame_support_traits_specs::frame_support_traits_dispatch,
    };

    pub(crate) fn frame_support_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
        args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();

        if path.starts_with("frame_support::BoundedVec::") {
            frame_support_bounded_vec_dispatch(transfer_function, callee_info, args_summary_keys);
        } else if path.starts_with("frame_support::traits::") {
            frame_support_traits_dispatch(transfer_function, callee_info, args_summary_keys);
        } else if path.starts_with("frame_support::dispatch::") {
            frame_support_dispatch_dispatch(transfer_function, callee_info, args_summary_keys);
        } else if path.starts_with("frame_support::weights::") {
            // ignore
        } else {
            match path {
                "frame_support::StorageMap::get" => {
                    // This is a macro generated getter
                    let value_type_size =
                        Type::from_mir_ty(transfer_function.tcx, callee_info.substs_ref.type_at(2));

                    transfer_function
                        .domain_state
                        .add_reads(value_type_size.get_size(transfer_function.tcx));

                    transfer_function
                        .domain_state
                        .add_steps(get_big_o_from_storage_size(
                            value_type_size.get_size(transfer_function.tcx),
                        ));
                }
                _ => unimplemented!(
                    "{} --- {:?}",
                    path,
                    transfer_function
                        .tcx
                        .mk_fn_def(callee_info.callee_def_id, callee_info.substs_ref)
                ),
            }
        }
    }
}

pub(crate) mod frame_support_bounded_vec_specs {

    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::{Cost, Symbolic},
    };

    pub(crate) fn frame_support_bounded_vec_dispatch<'tcx>(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();

        match path {
            "frame_support::BoundedVec::<T, S>::try_push" => {
                // call "try_push" https://docs.substrate.io/rustdocs/latest/frame_support/storage/bounded_vec/struct.BoundedVec.html#method.try_push
                // upperbound grow amortized by max size

                // extract the name of the type for readability
                let ty_name = callee_info.substs_ref.type_at(1).to_string();
                let ty_name = ty_name.split("::").last().unwrap();
                transfer_function
                    .domain_state
                    .add_steps(Cost::Symbolic(Symbolic::BigO(format!(
                        "VALUEOF({}::get())",
                        ty_name
                    ))));
            }
            "frame_support::BoundedVec::<T, S>::get_mut" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            "frame_support::BoundedVec::<T, S>::retain" => {
                // call "retain" https://docs.substrate.io/rustdocs/latest/frame_support/storage/bounded_vec/struct.BoundedVec.html#method.retain
                //TODO: multiply by cost of closure
                // extract the name of the type for readability
                let ty_name = callee_info.substs_ref.type_at(1).to_string();
                let ty_name = ty_name.split("::").last().unwrap();
                transfer_function
                    .domain_state
                    .add_steps(Cost::Symbolic(Symbolic::BigO(format!(
                        "VALUEOF({}::get())",
                        ty_name
                    ))));
            }
            _ => unimplemented!("{}", path),
        }
    }
}

pub(crate) mod frame_support_dispatch_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::{Cost, Symbolic},
    };

    pub(crate) fn frame_support_dispatch_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();

        match path {
            "frame_support::dispatch::UnfilteredDispatchable::dispatch_bypass_filter" => {
                // This specifications are far from optimal, but at least we see that another call is made
                let call_name = callee_info.substs_ref.type_at(0);
                transfer_function
                    .domain_state
                    .add_reads(Cost::Symbolic(Symbolic::ReadsOf(format!("{}", call_name))));
                transfer_function
                    .domain_state
                    .add_writes(Cost::Symbolic(Symbolic::WritesOf(format!("{}", call_name))));
                transfer_function
                    .domain_state
                    .add_events(Cost::Symbolic(Symbolic::EventsOf(format!("{}", call_name))));
                transfer_function
                    .domain_state
                    .add_steps(Cost::Symbolic(Symbolic::StepsOf(format!("{}", call_name))));
            }
            "frame_support::dispatch::Dispatchable::dispatch" => {
                // This specifications are far from optimal, but at least we see that another call is made
                let call_name = callee_info.substs_ref.type_at(0);
                transfer_function
                    .domain_state
                    .add_reads(Cost::Symbolic(Symbolic::ReadsOf(format!("{}", call_name))));
                transfer_function
                    .domain_state
                    .add_writes(Cost::Symbolic(Symbolic::WritesOf(format!("{}", call_name))));
                transfer_function
                    .domain_state
                    .add_events(Cost::Symbolic(Symbolic::EventsOf(format!("{}", call_name))));
                transfer_function
                    .domain_state
                    .add_steps(Cost::Symbolic(Symbolic::StepsOf(format!("{}", call_name))));
            }
            "frame_support::dispatch::GetDispatchInfo::get_dispatch_info" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            _ => unimplemented!(
                "{} --- {:?}",
                path,
                transfer_function
                    .tcx
                    .mk_fn_def(callee_info.callee_def_id, callee_info.substs_ref)
            ),
        }
    }
}

pub(crate) mod frame_support_traits_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::{get_big_o_from_storage_size, Cost, HasSize, Symbolic},
        types::Type,
    };

    pub(crate) fn frame_support_traits_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "frame_support::traits::EnsureOrigin::ensure_origin" => {
                // https://docs.substrate.io/rustdocs/latest/frame_support/traits/trait.EnsureOrigin.html#method.ensure_origin
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            "frame_support::traits::EnsureOrigin::try_origin" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            "frame_support::traits::Get::get" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            "frame_support::traits::ReservableCurrency::can_reserve"
            | "frame_support::traits::ReservableCurrency::slash_reserved"
            | "frame_support::traits::ReservableCurrency::reserved_balance"
            | "frame_support::traits::ReservableCurrency::reserve"
            | "frame_support::traits::ReservableCurrency::unreserve"
            | "frame_support::traits::ReservableCurrency::repatriate_reserved"
            | "frame_support::traits::Currency::transfer"
            | "frame_support::traits::fungible::Inspect::reducible_balance" => {
                let fn_name = path.split("::").last().unwrap();
                transfer_function
                    .domain_state
                    .add_reads(Cost::Symbolic(Symbolic::ReadsOf(format!(
                        "Currency::{}",
                        fn_name
                    ))));
                transfer_function
                    .domain_state
                    .add_writes(Cost::Symbolic(Symbolic::WritesOf(format!(
                        "Currency::{}",
                        fn_name
                    ))));
                transfer_function
                    .domain_state
                    .add_events(Cost::Symbolic(Symbolic::EventsOf(format!(
                        "Currency::{}",
                        fn_name
                    ))));
                transfer_function
                    .domain_state
                    .add_steps(Cost::Symbolic(Symbolic::StepsOf(format!(
                        "Currency::{}",
                        fn_name
                    ))));
            }
            "frame_support::traits::StoredMap::try_mutate_exists" => {
                let value_type_size =
                    Type::from_mir_ty(transfer_function.tcx, callee_info.substs_ref.type_at(2));

                transfer_function
                    .domain_state
                    .add_reads(value_type_size.get_size(transfer_function.tcx));
                transfer_function
                    .domain_state
                    .add_writes(value_type_size.get_size(transfer_function.tcx));
                transfer_function
                    .domain_state
                    .add_steps(get_big_o_from_storage_size(
                        value_type_size.get_size(transfer_function.tcx),
                    ));
            }
            "frame_support::traits::VestingSchedule::can_add_vesting_schedule"
            | "frame_support::traits::VestingSchedule::add_vesting_schedule" => {
                let fn_name = path.split("::").last().unwrap();
                transfer_function
                    .domain_state
                    .add_reads(Cost::Symbolic(Symbolic::ReadsOf(format!(
                        "VestingSchedule::{}",
                        fn_name
                    ))));
                transfer_function
                    .domain_state
                    .add_writes(Cost::Symbolic(Symbolic::WritesOf(format!(
                        "VestingSchedule::{}",
                        fn_name
                    ))));
                transfer_function
                    .domain_state
                    .add_events(Cost::Symbolic(Symbolic::EventsOf(format!(
                        "VestingSchedule::{}",
                        fn_name
                    ))));
                transfer_function
                    .domain_state
                    .add_steps(Cost::Symbolic(Symbolic::StepsOf(format!(
                        "VestingSchedule::{}",
                        fn_name
                    ))));
            }
            "frame_support::traits::OriginTrait::set_caller_from" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            _ => unimplemented!(
                "{} --- {:?}",
                path,
                transfer_function
                    .tcx
                    .mk_fn_def(callee_info.callee_def_id, callee_info.substs_ref)
            ),
        }
    }
}

pub(crate) mod frame_system_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::Cost,
    };
    use regex::Regex;

    pub(crate) fn frame_system_dispatch<'tcx>(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "frame_system::ensure_signed" | "frame_system::ensure_root" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            _ => {
                let block_number_regex =
                    Regex::new(r"frame_system::Pallet::<.*\s*(,.*)*>::block_number").unwrap();

                if block_number_regex.is_match(path) {
                    transfer_function.domain_state.add_steps(Cost::Concrete(1));
                } else {
                    unimplemented!("{}", path)
                }
            }
        }
    }
}

pub(crate) mod parity_scale_codec_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::{get_big_o_from_storage_size, HasSize},
        types::Type,
    };

    pub(crate) fn parity_scale_codec_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "parity_scale_codec::Encode::using_encoded" => {
                // arg is a hasher, complexity accounted for here
                transfer_function
                    .domain_state
                    .add_steps(get_big_o_from_storage_size(
                        Type::from_mir_ty(transfer_function.tcx, callee_info.substs_ref.type_at(0))
                            .get_size(transfer_function.tcx),
                    ));
            }
            "parity_scale_codec::Decode::decode" => {
                // Ideally should be parametrized on the length of the vector to decode,
                // but we can assume that is proportional to the type to be decoded to
                transfer_function
                    .domain_state
                    .add_steps(get_big_o_from_storage_size(
                        Type::from_mir_ty(transfer_function.tcx, callee_info.substs_ref.type_at(0))
                            .get_size(transfer_function.tcx),
                    ));
            }
            _ => unimplemented!("{}", path),
        }
    }
}

pub(crate) mod sp_io_specs {
    use crate::analysis::{cost_analysis::{CalleeInfo, SummaryKey, TransferFunction}, cost_language::{get_big_o_from_storage_size, HasSize}, types::Type};

    pub(crate) fn sp_io_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "sp_io::hashing::blake2_256" => {
                if callee_info.args.is_empty() {
                    // Function is passed as a closure and we don't know what will be hashed
                    // outter function must take care of this cost
                } else {
                    transfer_function
                    .domain_state
                    .add_steps(get_big_o_from_storage_size(
                        Type::from_mir_ty(transfer_function.tcx, transfer_function.get_local_type(&callee_info.args[0].place().unwrap()).get_ty())
                            .get_size(transfer_function.tcx),
                    ));
                }
            }
            _ => unimplemented!("{}", path),
        }
    }
}

pub(crate) mod sp_runtime_traits_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::{Cost, Symbolic},
    };

    use rustc_middle::ty::TyKind;

    pub(crate) fn sp_runtime_traits_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();

        match path {
            "sp_runtime::traits::Zero::zero" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            "sp_runtime::traits::Zero::is_zero" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            "sp_runtime::traits::One::one" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            "sp_runtime::traits::StaticLookup::lookup" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));

                // get the destination type to know what type is read from storage
                // return type is Result<read_type, error_type>
                let res = transfer_function
                    .get_local_type(&callee_info.destination.unwrap().0)
                    .get_ty()
                    .kind();
                let read_type = match res {
                    TyKind::Adt(_, substs_ref) => substs_ref.type_at(0),
                    _ => unreachable!(),
                };
                transfer_function
                    .domain_state
                    .add_reads(Cost::Symbolic(Symbolic::SizeOf(read_type.to_string())));
            }
            "sp_runtime::traits::StaticLookup::unlookup" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            "sp_runtime::traits::Saturating::saturating_add"
            | "sp_runtime::traits::Saturating::saturating_mul" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            "sp_runtime::traits::TrailingZeroInput::<'a>::new" => {
                // https://paritytech.github.io/substrate/master/sp_runtime/traits/struct.TrailingZeroInput.html#method.new
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            _ => unimplemented!(
                "{} --- {:?}",
                path,
                transfer_function
                    .tcx
                    .mk_fn_def(callee_info.callee_def_id, callee_info.substs_ref)
            ),
        }
    }
}

pub(crate) mod std_specs {

    use super::std_clone_specs::std_clone_dispatch;
    use super::std_cmp_specs::std_cmp_dispatch;
    use super::std_convert_specs::std_convert_dispatch;
    use super::std_default_specs::std_default_dispatch;
    use super::std_instrinsics_specs::std_intrinsics_dispatch;
    use super::std_iter_specs::std_iter_dispatch;
    use super::std_result_specs::std_result_dispatch;
    use super::std_slice_specs::std_slice_dispatch;
    use super::{std_alloc_specs::std_alloc_dispatch, std_ops_specs::std_ops_dispatch};
    use crate::analysis::cost_analysis::{CalleeInfo, SummaryKey, TransferFunction};

    pub(crate) fn std_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
        args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);

        if path.starts_with("std::alloc::") {
            std_alloc_dispatch(transfer_function, callee_info, args_summary_keys);
        } else if path.starts_with("std::clone::") {
            std_clone_dispatch(transfer_function, callee_info, args_summary_keys);
        } else if path.starts_with("std::cmp::") {
            std_cmp_dispatch(transfer_function, callee_info, args_summary_keys);
        } else if path.starts_with("std::convert::") {
            std_convert_dispatch(transfer_function, callee_info, args_summary_keys);
        } else if path.starts_with("std::default::") {
            std_default_dispatch(transfer_function, callee_info, args_summary_keys);
        } else if path.starts_with("std::intrinsics::") {
            std_intrinsics_dispatch(transfer_function, callee_info, args_summary_keys);
        } else if path.starts_with("std::iter::") {
            std_iter_dispatch(transfer_function, callee_info, args_summary_keys);
        } else if path.starts_with("std::ops::") {
            std_ops_dispatch(transfer_function, callee_info, args_summary_keys);
        } else if path.starts_with("std::result::") {
            std_result_dispatch(transfer_function, callee_info, args_summary_keys);
        } else if path.starts_with("std::slice::") {
            std_slice_dispatch(transfer_function, callee_info, args_summary_keys);
        } else {
            unimplemented!(
                "{} --- {:?}",
                path,
                transfer_function
                    .tcx
                    .mk_fn_def(callee_info.callee_def_id, callee_info.substs_ref)
            );
        }
    }
}

pub(crate) mod std_alloc_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn std_alloc_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "std::alloc::Allocator::deallocate" => {
                // deallocate boils down to libc::free
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            _ => unimplemented!(
                "{} --- {:?}",
                path,
                transfer_function
                    .tcx
                    .mk_fn_def(callee_info.callee_def_id, callee_info.substs_ref)
            ),
        }
    }
}

pub(crate) mod std_clone_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn std_clone_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        _callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        //TODO: linear if this is a vec or something similar
        transfer_function.domain_state.add_steps(Cost::Concrete(1));
    }
}

pub(crate) mod std_cmp_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn std_cmp_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        _callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        //TODO: linear if this is a vec or something similar
        transfer_function.domain_state.add_steps(Cost::Concrete(1));
    }
}

pub(crate) mod std_convert_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn std_convert_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "std::convert::Into::into" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            "std::convert::From::from" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            "std::convert::AsRef::as_ref" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            _ => unimplemented!(
                "{} --- {:?}",
                path,
                transfer_function
                    .tcx
                    .mk_fn_def(callee_info.callee_def_id, callee_info.substs_ref)
            ),
        }
    }
}

pub(crate) mod std_default_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn std_default_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "std::default::Default::default" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            _ => unimplemented!(
                "{} --- {:?}",
                path,
                transfer_function
                    .tcx
                    .mk_fn_def(callee_info.callee_def_id, callee_info.substs_ref)
            ),
        }
    }
}

pub(crate) mod std_instrinsics_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn std_intrinsics_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "std::intrinsics::size_of_val" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1))
            }
            "std::intrinsics::min_align_of_val" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1))
            }
            "std::intrinsics::transmute" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1))
            }
            "std::intrinsics::assert_inhabited" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1))
            }
            "std::intrinsics::saturating_add" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1))
            }
            _ => unimplemented!(
                "{} --- {:?}",
                path,
                transfer_function
                    .tcx
                    .mk_fn_def(callee_info.callee_def_id, callee_info.substs_ref)
            ),
        }
    }
}

pub(crate) mod std_iter_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction, AnalysisState},
    };

    pub(crate) fn std_iter_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        _callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        println!("{:?}", transfer_function.get_local_type(&_callee_info.args[0].place().unwrap()).get_ty().kind());
        *transfer_function.analysis_state.borrow_mut() = AnalysisState::Failure;
        println!("Iterators not supported yet");
        //panic!();
        
        /*match path {
            "std::iter::IntoIterator::into_iter" => {
                let underlying = transfer_function.get_local_type(&callee_info.args[0].place().unwrap()).get_ty();
                transfer_function
                    .domain_state
                    .add_steps(get_big_o_from_storage_size(
                        Type::from_mir_ty(transfer_function.tcx, underlying)
                            .get_size(transfer_function.tcx),
                    ));
                // Keep type with more information when switching to iterator
                let underlying = transfer_function.get_local_type(&callee_info.args[0].place().unwrap());
                transfer_function.local_types.borrow_mut()[callee_info.destination.unwrap().0.local] = underlying;
            }
            "std::iter::FromIterator::from_iter" => {
                // First arg is the iterator, then come the closures for map, filter, etc, in order of application
                println!("{:?}", transfer_function.get_local_type(&callee_info.args[1].place().unwrap()));
                panic!();
            }
            _ => unimplemented!(
                "{} --- {:?}",
                path,
                transfer_function
                    .tcx
                    .mk_fn_def(callee_info.callee_def_id, callee_info.substs_ref)
            ),
        }*/
    }
}

pub(crate) mod std_ops_specs {

    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn std_ops_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "std::ops::Try::branch" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            "std::ops::FromResidual::from_residual" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            "std::ops::Deref::deref" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));

                // Keep type with more information when derefencing
                let underlying = transfer_function.get_local_type(&callee_info.args[0].place().unwrap());
                transfer_function.local_types.borrow_mut()[callee_info.destination.unwrap().0.local] = underlying;
            }
            "std::ops::Mul::mul"
            | "std::ops::Add::add"
            | "std::ops::Sub::sub"
            | "std::ops::SubAssign::sub_assign" => {
                //TODO: linear or more if this is a vec or something similar
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            _ => {
                unimplemented!("{} --- {:?}", path, callee_info.substs_ref);
            }
        }
    }
}

pub(crate) mod std_result_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn std_result_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "std::result::unwrap_failed" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1))
            }
            _ => unimplemented!(
                "{} --- {:?}",
                path,
                transfer_function
                    .tcx
                    .mk_fn_def(callee_info.callee_def_id, callee_info.substs_ref)
            ),
        }
    }
}

pub(crate) mod std_slice_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::{Cost, get_big_o_from_storage_size, HasSize}, types::Type,
    };
    use rustc_middle::ty::TyKind;

    pub(crate) fn std_slice_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
        _args_summary_keys: Vec<Option<SummaryKey<'tcx>>>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "std::slice::SliceIndex::get" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            "std::slice::<impl [T]>::to_vec" => {
                let underlying = if let TyKind::Ref(_, ty, _) = transfer_function.get_local_type(&callee_info.args[0].place().unwrap()).get_ty().kind() {
                    *ty
                } else {
                    transfer_function.get_local_type(&callee_info.args[0].place().unwrap()).get_ty()
                };

                // Keep type with more information when converting to vec
                let source_ty = transfer_function.get_local_type(&callee_info.args[0].place().unwrap());
                transfer_function.local_types.borrow_mut()[callee_info.destination.unwrap().0.local] = source_ty;

                transfer_function
                    .domain_state
                    .add_steps(get_big_o_from_storage_size(
                        Type::from_mir_ty(transfer_function.tcx, underlying)
                            .get_size(transfer_function.tcx),
                    ));
            }
            _ => unimplemented!(
                "{} --- {:?}",
                path,
                transfer_function
                    .tcx
                    .mk_fn_def(callee_info.callee_def_id, callee_info.substs_ref)
            ),
        }
    }
}

pub(crate) mod storage_actions_specs {

    use crate::analysis::{
        cost_analysis::{CalleeInfo, SummaryKey, TransferFunction},
        cost_language::{get_big_o_from_storage_size, Cost, HasSize},
        pallet::{Field, StorageKind},
    };

    // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html
    // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageMap.html

    pub(crate) trait HasAccessCost {
        fn get_access_cost<'tcx>(
            &self,
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
            closure_summary_key: Option<SummaryKey<'tcx>>,
        );
    }

    impl HasAccessCost for Field {
        fn get_access_cost<'tcx>(
            &self,
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
            closure_summary_key: Option<SummaryKey<'tcx>>,
        ) {
            match &self.kind {
                StorageKind::StorageValue { .. } => StorageValueActions::get_access_cost(
                    self,
                    transfer_function,
                    callee_info,
                    closure_summary_key,
                ),
                StorageKind::StorageMap { .. } => StorageMapActions::get_access_cost(
                    self,
                    transfer_function,
                    callee_info,
                    closure_summary_key,
                ),

                StorageKind::StorageDoubleMap { .. } => StorageDoubleMapActions::get_access_cost(
                    self,
                    transfer_function,
                    callee_info,
                    closure_summary_key,
                ),
                StorageKind::StorageNMap { .. } => todo!(),
                StorageKind::CountedStorageMap { .. } => todo!(),
            }
        }
    }

    pub(crate) struct StorageValueActions {}

    impl StorageValueActions {
        pub fn get_access_cost<'tcx>(
            field: &Field,
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
            closure_summary_key: Option<SummaryKey<'tcx>>,
        ) {
            let action = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let action_short = action.split("::").last().unwrap();

            match action_short {
                "append" => todo!(),
                "decode_len" => todo!(),
                "exists" => {
                    // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.exists
                    transfer_function.domain_state.add_steps(Cost::Concrete(1));
                    // storage access
                    transfer_function
                        .domain_state
                        .add_reads(field.get_size(transfer_function.tcx));
                }
                "get" => {
                    // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.get
                    // decoding
                    transfer_function
                        .domain_state
                        .add_steps(get_big_o_from_storage_size(
                            field.get_size(transfer_function.tcx),
                        ));
                    // storage access
                    transfer_function
                        .domain_state
                        .add_reads(field.get_size(transfer_function.tcx));
                }
                "kill" => {
                    // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.kill
                    transfer_function.domain_state.add_steps(Cost::Concrete(1));
                    // Write None to database
                    transfer_function.domain_state.add_writes(Cost::Concrete(1));
                }
                "mutate" => {
                    // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.mutate
                    // decoding/encoding
                    transfer_function
                        .domain_state
                        .add_steps(get_big_o_from_storage_size(
                            field.get_size(transfer_function.tcx),
                        ));

                    // storage access
                    transfer_function
                        .domain_state
                        .add_reads(field.get_size(transfer_function.tcx));

                    // account for closure complexity
                    let closure_summary_key = closure_summary_key.unwrap();
                    let closure_cost = transfer_function
                        .summaries
                        .borrow()
                        .get(&closure_summary_key)
                        .unwrap()
                        .clone()
                        .unwrap();
                    transfer_function.domain_state.inter_join(&closure_cost);

                    // storage access
                    transfer_function
                        .domain_state
                        .add_writes(field.get_size(transfer_function.tcx));
                }
                "put" => {
                    // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.put
                    transfer_function
                        .domain_state
                        .add_steps(get_big_o_from_storage_size(
                            field.get_size(transfer_function.tcx),
                        ));
                    // storage access
                    transfer_function
                        .domain_state
                        .add_writes(field.get_size(transfer_function.tcx));
                }
                "set" => {
                    // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.set
                    // encoding
                    transfer_function
                        .domain_state
                        .add_steps(get_big_o_from_storage_size(
                            field.get_size(transfer_function.tcx),
                        ));
                    // storage access
                    transfer_function
                        .domain_state
                        .add_writes(field.get_size(transfer_function.tcx));
                }
                "take" => todo!(),
                "translate" => todo!(),
                "try_append" => todo!(),
                "try_get" => {
                    // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.try_get
                    transfer_function
                        .domain_state
                        .add_steps(get_big_o_from_storage_size(
                            field.get_size(transfer_function.tcx),
                        ));
                    transfer_function
                        .domain_state
                        .add_reads(field.get_size(transfer_function.tcx));
                }
                "try_mutate" => {
                    // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.try_mutate
                    // decoding/encoding depends on the actual length of what is stored

                    transfer_function
                        .domain_state
                        .add_steps(get_big_o_from_storage_size(
                            field.get_size(transfer_function.tcx),
                        ));

                    // storage access
                    transfer_function
                        .domain_state
                        .add_reads(field.get_size(transfer_function.tcx));

                    // account for closure complexity
                    let closure_summary_key = closure_summary_key.unwrap();
                    let closure_cost = transfer_function
                        .summaries
                        .borrow()
                        .get(&closure_summary_key)
                        .unwrap()
                        .clone()
                        .unwrap();
                    transfer_function.domain_state.inter_join(&closure_cost);

                    // storage access
                    transfer_function
                        .domain_state
                        .add_writes(field.get_size(transfer_function.tcx));
                }
                _ => unimplemented!(
                    "{}",
                    transfer_function
                        .tcx
                        .def_path_str(callee_info.callee_def_id)
                ),
            };
        }
    }

    pub(crate) struct StorageMapActions {}

    impl StorageMapActions {
        pub fn get_access_cost<'tcx>(
            field: &Field,
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
            closure_summary_key: Option<SummaryKey<'tcx>>,
        ) {
            let action = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let action_short = action.split("::").last().unwrap();

            match action_short {
                "append" => todo!(),
                "contains_key" => {
                    // call "contains_key" https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#236
                    transfer_function.domain_state.add_steps(Cost::Concrete(1));
                    transfer_function
                        .domain_state
                        .add_reads(field.get_size(transfer_function.tcx));
                }
                "decode_len" => todo!(),
                "drain" => todo!(),
                "get" => {
                    // call "get" https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#240
                    transfer_function
                        .domain_state
                        .add_steps(get_big_o_from_storage_size(
                            field.get_size(transfer_function.tcx),
                        ));
                    transfer_function
                        .domain_state
                        .add_reads(field.get_size(transfer_function.tcx));
                }
                "insert" => {
                    // call "insert" https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#248
                    transfer_function
                        .domain_state
                        .add_steps(get_big_o_from_storage_size(
                            field.get_size(transfer_function.tcx),
                        ));
                    transfer_function
                        .domain_state
                        .add_writes(field.get_size(transfer_function.tcx));
                }
                "iter" => todo!(),
                "iter_from" => todo!(),
                "iter_keys" => todo!(),
                "iter_key_from" => todo!(),
                "iter_values" => todo!(),
                "migrate_key" => todo!(),
                "mutate" => {
                    // call "mutate" https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#256
                    transfer_function
                        .domain_state
                        .add_steps(get_big_o_from_storage_size(
                            field.get_size(transfer_function.tcx),
                        ));
                    transfer_function
                        .domain_state
                        .add_reads(field.get_size(transfer_function.tcx));

                    // account for closure complexity
                    let closure_summary_key = closure_summary_key.unwrap();
                    let closure_cost = transfer_function
                        .summaries
                        .borrow()
                        .get(&closure_summary_key)
                        .unwrap()
                        .clone()
                        .unwrap();
                    transfer_function.domain_state.inter_join(&closure_cost);

                    transfer_function
                        .domain_state
                        .add_writes(field.get_size(transfer_function.tcx));
                }
                "mutate_exists" => todo!(),
                "remove" => {
                    // call "remove" https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#248
                    transfer_function.domain_state.add_steps(Cost::Concrete(1));
                    // Write "None" to storage
                    transfer_function.domain_state.add_writes(Cost::Concrete(1));
                }
                "remove_all" => todo!(),
                "swap" => todo!(),
                "take" => {
                    // call "take" https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#303
                    transfer_function
                        .domain_state
                        .add_steps(get_big_o_from_storage_size(
                            field.get_size(transfer_function.tcx),
                        ));

                    transfer_function
                        .domain_state
                        .add_reads(field.get_size(transfer_function.tcx));

                    // Write "None" to storage
                    transfer_function.domain_state.add_writes(Cost::Concrete(1));
                }
                "translate" => todo!(),
                "translate_values" => todo!(),
                "try_append" => todo!(),
                "try_get" => {
                    // call "try_get" https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#244
                    transfer_function
                        .domain_state
                        .add_steps(get_big_o_from_storage_size(
                            field.get_size(transfer_function.tcx),
                        ));
                    transfer_function
                        .domain_state
                        .add_reads(field.get_size(transfer_function.tcx));
                }
                "try_mutate" => {
                    // call try_mutate https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#286
                    transfer_function
                        .domain_state
                        .add_steps(get_big_o_from_storage_size(
                            field.get_size(transfer_function.tcx),
                        ));
                    transfer_function
                        .domain_state
                        .add_reads(field.get_size(transfer_function.tcx));

                    // account for closure complexity
                    let closure_summary_key = closure_summary_key.unwrap();
                    let closure_cost = transfer_function
                        .summaries
                        .borrow()
                        .get(&closure_summary_key)
                        .unwrap()
                        .clone()
                        .unwrap();
                    transfer_function.domain_state.inter_join(&closure_cost);

                    transfer_function
                        .domain_state
                        .add_writes(field.get_size(transfer_function.tcx));
                }
                "try_mutate_exists" => todo!(),
                _ => unimplemented!(
                    "{}",
                    transfer_function
                        .tcx
                        .def_path_str(callee_info.callee_def_id)
                ),
            };
        }
    }

    pub(crate) struct StorageDoubleMapActions {}

    impl StorageDoubleMapActions {
        pub fn get_access_cost<'tcx>(
            _field: &Field,
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
            _closure_summary_key: Option<SummaryKey<'tcx>>,
        ) {
            let action = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let action_short = action.split("::").last().unwrap();

            match action_short {
                "append" => todo!(),
                "contains_key" => todo!(),
                "decode_len" => todo!(),
                "drain" => todo!(),
                "drain_prefix" => todo!(),
                "get" => todo!(),
                "insert" => todo!(),
                "iter" => todo!(),
                "iter_from" => todo!(),
                "iter_key_prefix" => todo!(),
                "iter_key_prefix_from" => todo!(),
                "iter_keys" => todo!(),
                "iter_keys_from" => todo!(),
                "iter_prefix" => todo!(),
                "iter_prefix_from" => todo!(),
                "iter_prefix_values" => todo!(),
                "iter_values" => todo!(),
                "migrate_keys" => todo!(),
                "mutate" => todo!(),
                "mutate_exists" => todo!(),
                "remove" => todo!(),
                "remove_all" => todo!(),
                "remove_prefix" => todo!(),
                "swap" => todo!(),
                "take" => todo!(),
                "translate" => todo!(),
                "translate_values" => todo!(),
                "try_append" => todo!(),
                "try_get" => todo!(),
                "try_mutate" => todo!(),
                "try_mutate_exists" => todo!(),
                _ => unimplemented!(
                    "{}",
                    transfer_function
                        .tcx
                        .def_path_str(callee_info.callee_def_id)
                ),
            };
        }
    }
}
