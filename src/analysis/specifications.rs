use crate::analysis::cost_analysis::{CalleeInfo, TransferFunction};

use self::frame_support_specs::frame_support_dispatch;
use self::frame_system_specs::frame_system_dispatch;
use self::parity_scale_codec_specs::parity_scale_codec_dispatch;
use self::sp_io_specs::sp_io_dispatch;
use self::sp_runtime_traits_specs::sp_runtime_traits_dispatch;
use self::std_specs::std_dispatch;

pub(crate) fn dispatch_to_specifications<'tcx>(
    transfer_function: &mut TransferFunction<'tcx, '_, '_>,
    callee_info: CalleeInfo<'tcx>,
) {
    let path = transfer_function
        .tcx
        .def_path_str(callee_info.callee_def_id);

    if path.starts_with("frame_support::") {
        frame_support_dispatch(transfer_function, callee_info);
    } else if path.starts_with("frame_system::") {
        frame_system_dispatch(transfer_function, callee_info);
    } else if path.starts_with("parity_scale_codec::") {
        parity_scale_codec_dispatch(transfer_function, callee_info);
    } else if path.starts_with("sp_io::") {
        sp_io_dispatch(transfer_function, callee_info);
    } else if path.starts_with("sp_runtime::traits::") {
        sp_runtime_traits_dispatch(transfer_function, callee_info);
    } else if path.starts_with("std::") {
        std_dispatch(transfer_function, callee_info);
    } else if path.starts_with("weights::WeightInfo::") {
        // Ignore
    } else {
        unimplemented!("{} --- {:?}", path, callee_info.func);
    }
}

pub(crate) mod frame_support_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::{get_big_o_from_storage_size, HasSize},
        types::Type,
    };

    use super::{
        frame_support_bounded_vec_specs::frame_support_bounded_vec_dispatch,
        frame_support_traits_specs::frame_support_traits_dispatch,
    };

    pub(crate) fn frame_support_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();

        if path.starts_with("frame_support::BoundedVec::") {
            frame_support_bounded_vec_dispatch(transfer_function, callee_info);
        } else if path.starts_with("frame_support::traits::") {
            frame_support_traits_dispatch(transfer_function, callee_info);
        } else {
            match path {
                "frame_support::StorageMap::get" => {
                    // This is a macro generated getter
                    let value_type_size =
                        Type::from_mir_ty(transfer_function.tcx, callee_info.substs_ref.type_at(2));

                    transfer_function
                        .domain_state
                        .add_reads(value_type_size.get_size(&transfer_function.tcx));

                    transfer_function
                        .domain_state
                        .add_steps(get_big_o_from_storage_size(
                            value_type_size.get_size(&transfer_function.tcx),
                        ));
                }
                _ => unimplemented!("{} --- {:?}", path, callee_info.func),
            }
        }
    }
}

pub(crate) mod frame_support_bounded_vec_specs {

    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::{Cost, Symbolic},
    };

    pub(crate) fn frame_support_bounded_vec_dispatch<'tcx>(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
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

pub(crate) mod frame_support_traits_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::{get_big_o_from_storage_size, Cost, HasSize, Symbolic},
        types::Type,
    };

    pub(crate) fn frame_support_traits_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: CalleeInfo<'tcx>,
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
                    .add_reads(value_type_size.get_size(&transfer_function.tcx));
                transfer_function
                    .domain_state
                    .add_writes(value_type_size.get_size(&transfer_function.tcx));
                transfer_function
                    .domain_state
                    .add_steps(get_big_o_from_storage_size(
                        value_type_size.get_size(&transfer_function.tcx),
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
            _ => {
                unimplemented!("{} --- {:?}", path, callee_info.func);
            }
        }
    }
}

pub(crate) mod frame_system_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::Cost,
    };
    use regex::Regex;

    pub(crate) fn frame_system_dispatch<'tcx>(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
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
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn parity_scale_codec_dispatch<'tcx>(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "parity_scale_codec::Encode::using_encoded" => {
                println!("{:?}", callee_info.substs_ref);
                panic!();
            }
            _ => unimplemented!("{}", path),
        }
    }
}

pub(crate) mod sp_io_specs {
    use crate::analysis::cost_analysis::{CalleeInfo, TransferFunction};

    pub(crate) fn sp_io_dispatch(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            _ => unimplemented!("{}", path),
        }
    }
}

pub(crate) mod sp_runtime_traits_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::{Cost, Symbolic},
    };

    use rustc_middle::ty::TyKind;

    pub(crate) fn sp_runtime_traits_dispatch<'tcx>(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
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
            _ => {
                unimplemented!(
                    "{} --- {:?} --- {:?}",
                    path,
                    callee_info.func,
                    callee_info.substs_ref,
                );
            }
        }
    }
}

pub(crate) mod std_specs {

    use super::std_clone_specs::std_clone_dispatch;
    use super::std_cmp_specs::std_cmp_dispatch;
    use super::std_convert_specs::std_convert_dispatch;
    use super::std_default_specs::std_default_dispatch;
    use super::std_instrinsics_specs::std_intrinsics_dispatch;
    use super::std_result_specs::std_result_dispatch;
    use super::std_slice_specs::std_slice_dispatch;
    use super::{std_alloc_specs::std_alloc_dispatch, std_ops_specs::std_ops_dispatch};
    use crate::analysis::cost_analysis::{CalleeInfo, TransferFunction};

    pub(crate) fn std_dispatch<'tcx>(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);

        if path.starts_with("std::alloc::") {
            std_alloc_dispatch(transfer_function, callee_info);
        } else if path.starts_with("std::clone::") {
            std_clone_dispatch(transfer_function, callee_info);
        } else if path.starts_with("std::cmp::") {
            std_cmp_dispatch(transfer_function, callee_info);
        } else if path.starts_with("std::convert::") {
            std_convert_dispatch(transfer_function, callee_info);
        } else if path.starts_with("std::default::") {
            std_default_dispatch(transfer_function, callee_info);
        } else if path.starts_with("std::intrinsics::") {
            std_intrinsics_dispatch(transfer_function, callee_info);
        } else if path.starts_with("std::ops::") {
            std_ops_dispatch(transfer_function, callee_info);
        } else if path.starts_with("std::result::") {
            std_result_dispatch(transfer_function, callee_info);
        } else if path.starts_with("std::slice::") {
            std_slice_dispatch(transfer_function, callee_info);
        } else {
            unimplemented!("{} --- {:?}", path, callee_info.func);
        }
    }
}

pub(crate) mod std_alloc_specs {
    use crate::analysis::cost_analysis::{CalleeInfo, TransferFunction};

    pub(crate) fn std_alloc_dispatch<'tcx>(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "std::alloc::Allocator::deallocate" => {
                println!("DEALLOCATE");
                println!(
                    "{:?}",
                    transfer_function
                        .local_types
                        .borrow()
                        .get(callee_info.args[0].place().unwrap().local)
                        .unwrap()
                );
                //TODO: specs
                //panic!()
            }
            _ => unimplemented!("{} --- {:?}", path, callee_info.func),
        }
    }
}

pub(crate) mod std_clone_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn std_clone_dispatch<'tcx>(
        transfer_function: &mut TransferFunction,
        _callee_info: CalleeInfo<'tcx>,
    ) {
        //TODO: linear if this is a vec or something similar
        transfer_function.domain_state.add_steps(Cost::Concrete(1));
    }
}

pub(crate) mod std_cmp_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn std_cmp_dispatch(
        transfer_function: &mut TransferFunction,
        _callee_info: CalleeInfo,
    ) {
        //TODO: linear if this is a vec or something similar
        transfer_function.domain_state.add_steps(Cost::Concrete(1));
    }
}

pub(crate) mod std_convert_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn std_convert_dispatch(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo,
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
            _ => {
                unimplemented!("{} --- {:?}", path, callee_info.func);
            }
        }
    }
}

pub(crate) mod std_default_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn std_default_dispatch<'tcx>(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "std::default::Default::default" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            _ => {
                unimplemented!("{} --- {:?}", path, callee_info.func);
            }
        }
    }
}

pub(crate) mod std_instrinsics_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn std_intrinsics_dispatch<'tcx>(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
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
            _ => unimplemented!("{} --- {:?}", path, callee_info.func),
        }
    }
}

pub(crate) mod std_ops_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn std_ops_dispatch<'tcx>(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
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
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::Cost,
    };

    pub(crate) fn std_result_dispatch<'tcx>(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "std::result::unwrap_failed" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1))
            }
            _ => unimplemented!("{} --- {:?}", path, callee_info.func),
        }
    }
}

pub(crate) mod std_slice_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_language::Cost,
    };
    use rustc_middle::ty::TyKind;

    pub(crate) fn std_slice_dispatch<'tcx>(
        transfer_function: &mut TransferFunction,
        callee_info: CalleeInfo<'tcx>,
    ) {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "std::slice::SliceIndex::get" => {
                transfer_function.domain_state.add_steps(Cost::Concrete(1));
            }
            "std::slice::hack::ConvertVec::to_vec" => {
                //TODO:
                let ty = if let TyKind::Ref(_, ty, _) = transfer_function
                    .get_local_type(&callee_info.args[0].place().unwrap())
                    .get_ty()
                    .kind()
                {
                    if let TyKind::Slice(ty) = ty.kind() {
                        ty
                    } else {
                        unreachable!()
                    }
                } else {
                    unreachable!()
                };
                println!("{:?}", ty);
                panic!();
            }
            _ => unimplemented!(
                "{} --- {:?} --- {:?}",
                path,
                callee_info.func,
                callee_info.substs_ref,
            ),
        }
    }
}

pub(crate) mod storage_actions_specs {

    use crate::analysis::{
        cost_language::{get_big_o_from_storage_size, Cost, HasSize},
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
                    StorageValueActions::get_access_cost(tcx, self, action)
                }
                StorageKind::StorageMap { .. } => {
                    StorageMapActions::get_access_cost(tcx, self, action)
                }
                StorageKind::StorageDoubleMap { .. } => {
                    StorageDoubleMapActions::get_access_cost(tcx, self, action)
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
                let mut steps = Cost::default();
                let mut reads = Cost::default();
                let mut writes = Cost::default();

                match Self::from(action_short) {
                    StorageValueActions::Append => todo!(),
                    StorageValueActions::DecodeLen => todo!(),
                    StorageValueActions::Exists => {
                        // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.exists
                        steps = steps + Cost::Concrete(1);
                        // storage access
                        reads = reads + field.get_size(&tcx);
                    }
                    StorageValueActions::Get => {
                        // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.get
                        // decoding
                        steps = steps + get_big_o_from_storage_size(field.get_size(&tcx));
                        // storage access
                        reads = reads + field.get_size(&tcx);
                    }
                    StorageValueActions::Kill => {
                        // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.kill
                        steps = steps + Cost::Concrete(1);
                        // Write None to database
                        writes = writes + Cost::Concrete(1);
                    }
                    StorageValueActions::Mutate => {
                        // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.mutate
                        // decoding/encoding
                        steps = steps + get_big_o_from_storage_size(field.get_size(&tcx));

                        // storage access
                        reads = reads + field.get_size(&tcx);
                        // storage access
                        writes = writes + field.get_size(&tcx);
                    }
                    StorageValueActions::Put => {
                        // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.put
                        steps = steps + get_big_o_from_storage_size(field.get_size(&tcx));
                        // storage access
                        writes = writes + field.get_size(&tcx);
                    }
                    StorageValueActions::Set => {
                        // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.set
                        // encoding
                        steps = steps + get_big_o_from_storage_size(field.get_size(&tcx));
                        // storage access
                        writes = writes + field.get_size(&tcx);
                    }
                    StorageValueActions::Take => todo!(),
                    StorageValueActions::Translate => todo!(),
                    StorageValueActions::TryAppend => todo!(),
                    StorageValueActions::TryGet => {
                        // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.try_get
                        steps = steps + get_big_o_from_storage_size(field.get_size(&tcx));
                        reads = reads + field.get_size(&tcx);
                    }
                    StorageValueActions::TryMutate => {
                        // https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html#method.try_mutate
                        // decoding/encoding depends on the actual length of what is stored

                        steps = steps + get_big_o_from_storage_size(field.get_size(&tcx));

                        // storage access
                        reads = reads + field.get_size(&tcx);
                        // storage access
                        writes = writes + field.get_size(&tcx);
                    }
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
                let mut steps = Cost::default();
                let mut reads = Cost::default();
                let mut writes = Cost::default();

                match Self::from(action_short) {
                    StorageMapActions::Append => todo!(),
                    StorageMapActions::ContainsKey => {
                        // call "contains_key" https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#236
                        steps = steps + Cost::Concrete(1);
                        reads = reads + field.get_size(&tcx);
                    }
                    StorageMapActions::DecodeLen => todo!(),
                    StorageMapActions::Drain => todo!(),
                    StorageMapActions::Get => {
                        // call "get" https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#240
                        steps = steps + get_big_o_from_storage_size(field.get_size(&tcx));
                        reads = reads + field.get_size(&tcx);
                    }
                    StorageMapActions::Insert => {
                        // call "insert" https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#248
                        steps = steps + get_big_o_from_storage_size(field.get_size(&tcx));
                        writes = writes + field.get_size(&tcx);
                    }
                    StorageMapActions::Iter => todo!(),
                    StorageMapActions::IterFrom => todo!(),
                    StorageMapActions::IterKeys => todo!(),
                    StorageMapActions::IterKeysFrom => todo!(),
                    StorageMapActions::IterValues => todo!(),
                    StorageMapActions::MigrateKey => todo!(),
                    StorageMapActions::Mutate => {
                        // call "mutate" https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#256
                        steps = steps + get_big_o_from_storage_size(field.get_size(&tcx));
                        reads = reads + field.get_size(&tcx);
                        writes = writes + field.get_size(&tcx);
                    }
                    StorageMapActions::MutateExists => todo!(),
                    StorageMapActions::Remove => {
                        // call "remove" https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#248
                        steps = steps + Cost::Concrete(1);
                        writes = writes + Cost::Concrete(1);
                    }
                    StorageMapActions::RemoveAll => todo!(),
                    StorageMapActions::Swap => todo!(),
                    StorageMapActions::Take => {
                        // call "take" https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#303
                        steps = steps + get_big_o_from_storage_size(field.get_size(&tcx));

                        reads = reads + field.get_size(&tcx);

                        // Write "None" to storage
                        writes = writes + Cost::Concrete(1);
                    }
                    StorageMapActions::Translate => todo!(),
                    StorageMapActions::TranslateValues => todo!(),
                    StorageMapActions::TryAppend => todo!(),
                    StorageMapActions::TryGet => {
                        // call "try_get" https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#244
                        steps = steps + get_big_o_from_storage_size(field.get_size(&tcx));
                        reads = reads + field.get_size(&tcx);
                    }
                    StorageMapActions::TryMutate => {
                        // call try_mutate https://docs.substrate.io/rustdocs/latest/src/frame_support/storage/generator/map.rs.html#286
                        steps = steps + get_big_o_from_storage_size(field.get_size(&tcx));
                        reads = reads + field.get_size(&tcx);
                        writes = writes + field.get_size(&tcx);
                    }
                    StorageMapActions::TryMutateExists => (),
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
                let _substs_ref =
                    if let TyKind::Adt(_, substs_ref) = tcx.type_of(field.def_id).kind() {
                        substs_ref
                    } else {
                        unreachable!()
                    };

                let mut _steps = Cost::default();
                let mut _reads = Cost::default();
                let mut _writes = Cost::default();

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
                Some(AccessCost::new(_reads, _writes, _steps))
            }
        }
    }
}
