use self::{
    core_specs::try_core_dispatch, custom_specs::try_custom_dispatch,
    frame_support_specs::try_frame_support_dispatch, frame_system_specs::try_frame_system_dispatch,
    pallet_specs::try_pallet_dispatch, parity_scale_codec_specs::try_parity_scale_codec_dispatch,
    sp_io_specs::try_sp_io_dispatch, sp_runtime_specs::try_sp_runtime_dispatch,
    std_specs::try_std_dispatch,
};

use super::{
    cost_analysis::{CalleeInfo, TransferFunction},
    cost_domain::ExtendedCostAnalysisDomain,
};

pub(super) fn try_dispatch_to_specifications<'tcx>(
    transfer_function: &mut TransferFunction<'tcx, '_, '_>,
    callee_info: &CalleeInfo<'tcx>,
) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
    let path = transfer_function
        .tcx
        .def_path_str(callee_info.callee_def_id);

    if path.starts_with("weights::") || path.starts_with("frame_support::weights::") {
        // Ignore
        Some((*transfer_function.state).clone())
    } else if path.starts_with("core::") {
        try_core_dispatch(transfer_function, callee_info)
    } else if path.starts_with("frame_support::") {
        try_frame_support_dispatch(transfer_function, callee_info)
    } else if path.starts_with("frame_system::") {
        try_frame_system_dispatch(transfer_function, callee_info)
    } else if path.starts_with("pallet::Pallet::") {
        try_pallet_dispatch(transfer_function, callee_info)
    } else if path.starts_with("parity_scale_codec::") {
        try_parity_scale_codec_dispatch(transfer_function, callee_info)
    } else if path.starts_with("sp_io::") {
        try_sp_io_dispatch(transfer_function, callee_info)
    } else if path.starts_with("sp_runtime::") {
        try_sp_runtime_dispatch(transfer_function, callee_info)
    } else if path.starts_with("std::") {
        try_std_dispatch(transfer_function, callee_info)
    } else {
        try_custom_dispatch(transfer_function, callee_info)
    }
}

pub(super) mod core_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_domain::ExtendedCostAnalysisDomain,
    };

    use self::core_slice_specs::try_core_slice_dispatch;

    pub(crate) fn try_core_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: &CalleeInfo<'tcx>,
    ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);

        if path.starts_with("core::slice::") {
            try_core_slice_dispatch(transfer_function, callee_info)
        } else {
            None
        }
    }

    mod core_slice_specs {
        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::{ExtendedCostAnalysisDomain, LocalInfo},
            cost_language::{cost_to_big_o, Cost, CostParameter},
        };
        use rustc_middle::ty::TyKind;

        pub(super) fn try_core_slice_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();
            match path {
                "core::slice::<impl [T]>::binary_search_by" => {
                    let vector_element_type = if let TyKind::Adt(_, substs_ref) =
                        callee_info.args_type_info[0].get_ty().kind()
                    {
                        substs_ref.type_at(0)
                    } else {
                        unreachable!()
                    };

                    let closure_adt = callee_info.args_type_info[1].clone();
                    let (closure_fn_ptr, closure_substs_ref) =
                        if let TyKind::Closure(def_id, substs_ref) = closure_adt.get_ty().kind() {
                            (*def_id, substs_ref)
                        } else if let TyKind::FnDef(def_id, substs_ref) =
                            closure_adt.get_ty().kind()
                        {
                            (*def_id, substs_ref)
                        } else {
                            unreachable!();
                        };

                    // Account for closure call
                    let closure_call_simulation = CalleeInfo {
                        location: None,
                        args_type_info: vec![LocalInfo::new(
                            vector_element_type,
                            transfer_function.tcx,
                            None,
                            transfer_function.fresh_var_id.clone(),
                        )],
                        caller_args_operands: None,
                        destination: callee_info.destination,
                        callee_def_id: closure_fn_ptr,
                        substs_ref: closure_substs_ref,
                    };

                    let mut closure_analysis_result =
                        transfer_function.fn_call_analysis(closure_call_simulation, true);

                    let vec_big_o_size = cost_to_big_o(
                        callee_info.args_type_info[0].get_size(transfer_function.tcx),
                    );

                    let binary_search_complexity =
                        Cost::Parameter(CostParameter::Log(format!("{}", vec_big_o_size)));

                    // Then multiply it by complexity
                    closure_analysis_result.cost_big_o_mul(binary_search_complexity);

                    transfer_function.state.inter_join(&closure_analysis_result);
                    Some((*transfer_function.state).clone())
                }
                "core::slice::<impl [T]>::binary_search_by_key" => {
                    let vector_element_type = if let TyKind::Adt(_, substs_ref) =
                        callee_info.args_type_info[0].get_ty().kind()
                    {
                        substs_ref.type_at(0)
                    } else {
                        unreachable!()
                    };

                    let closure_adt = callee_info.args_type_info[2].clone();
                    let (closure_fn_ptr, closure_substs_ref) =
                        if let TyKind::Closure(def_id, substs_ref) = closure_adt.get_ty().kind() {
                            (*def_id, substs_ref)
                        } else if let TyKind::FnDef(def_id, substs_ref) =
                            closure_adt.get_ty().kind()
                        {
                            (*def_id, substs_ref)
                        } else {
                            unreachable!("{:#?}", callee_info);
                        };

                    // Account for closure call
                    let closure_call_simulation = CalleeInfo {
                        location: None,
                        args_type_info: vec![LocalInfo::new(
                            vector_element_type,
                            transfer_function.tcx,
                            None,
                            transfer_function.fresh_var_id.clone(),
                        )],
                        caller_args_operands: None,
                        destination: callee_info.destination,
                        callee_def_id: closure_fn_ptr,
                        substs_ref: closure_substs_ref,
                    };

                    let mut closure_analysis_result =
                        transfer_function.fn_call_analysis(closure_call_simulation, true);

                    let vec_big_o_size = cost_to_big_o(
                        callee_info.args_type_info[0].get_size(transfer_function.tcx),
                    );

                    let binary_search_complexity =
                        Cost::Parameter(CostParameter::Log(format!("{}", vec_big_o_size)));

                    // Then multiply it by complexity
                    closure_analysis_result.cost_big_o_mul(binary_search_complexity);

                    transfer_function.state.inter_join(&closure_analysis_result);
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }
}

pub(super) mod custom_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_domain::ExtendedCostAnalysisDomain,
        cost_language::Cost,
    };

    pub(super) fn try_custom_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: &CalleeInfo<'tcx>,
    ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "<impl pallet::Pallet<T>>::ensure_sorted_and_insert" => {
                let vec = callee_info.args_type_info[0].clone();

                let steps_of_ensure_sorted_and_insert =
                    Cost::BigO(Box::new((vec.length_of.borrow().clone().unwrap()).clone()));

                // Get vec and update length
                let vec_place = callee_info.caller_args_operands.clone().unwrap()[0].place().unwrap();
                assert!(vec_place.projection.is_empty());
                transfer_function.state.locals_info[vec_place.local].length_of_add_one();
                transfer_function.state.locals_info[vec_place.local].fill_with_inner_size(transfer_function.tcx);

                // Propagate vec to the destination place
                let dest_place = callee_info.destination.unwrap();
                // dest place is a Result and we insert in the first member
                assert!(transfer_function.state.locals_info[dest_place.local].get_members().len() == 2);
                let vec_local_info = transfer_function.state.get_local_info_for_place(&vec_place).unwrap();
                transfer_function.state.locals_info[dest_place.local].set_member(0, vec_local_info);
                transfer_function.state.locals_info[dest_place.local].fill_member_with_inner_size(0, transfer_function.tcx);

                transfer_function
                    .state
                    .add_steps(steps_of_ensure_sorted_and_insert);

                Some((*transfer_function.state).clone())
            }
            _ => None,
        }
    }
}

pub(super) mod frame_support_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_domain::ExtendedCostAnalysisDomain,
    };

    use self::{
        frame_support_bounded_vec_specs::try_frame_support_bounded_vec_dispatch,
        frame_support_dispatch_specs::try_frame_support_dispatch_dispatch,
        frame_support_macro_generated_storage_getter_specs::try_frame_support_macro_generated_storage_getter,
        frame_support_pallet_prelude_specs::try_frame_support_pallet_prelude_dispatch,
        frame_support_specs_traits_specs::try_frame_support_traits_dispatch,
    };

    pub(super) fn try_frame_support_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: &CalleeInfo<'tcx>,
    ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);

        if path.starts_with("frame_support::BoundedVec::<T, S>::") {
            try_frame_support_bounded_vec_dispatch(transfer_function, callee_info)
        } else if path.starts_with("frame_support::dispatch::") {
            try_frame_support_dispatch_dispatch(transfer_function, callee_info)
        } else if path.starts_with("frame_support::pallet_prelude::") {
            try_frame_support_pallet_prelude_dispatch(transfer_function, callee_info)
        } else if path == "frame_support::StorageMap::get"
            || path == "frame_support::StorageValue::get"
        {
            try_frame_support_macro_generated_storage_getter(transfer_function, callee_info)
        } else if path.starts_with("frame_support::traits::") {
            try_frame_support_traits_dispatch(transfer_function, callee_info)
        } else {
            None
        }
    }

    mod frame_support_bounded_vec_specs {
        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::{ExtendedCostAnalysisDomain, LocalInfo},
            cost_language::{Cost, CostParameter},
        };
        use rustc_middle::ty::TyKind;

        pub(super) fn try_frame_support_bounded_vec_dispatch<'tcx>(
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

                    // Update length
                    let bounded_vec_place = callee_info.caller_args_operands.clone().unwrap()[0]
                        .place()
                        .unwrap();
                    assert!(bounded_vec_place.projection.is_empty());
                    transfer_function.state.locals_info[bounded_vec_place.local]
                        .length_of_add_one();

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

                    let vector_element_type = if let TyKind::Adt(_, substs_ref) =
                        callee_info.args_type_info[0].get_ty().kind()
                    {
                        substs_ref.type_at(0)
                    } else {
                        unreachable!()
                    };

                    let closure_adt = callee_info.args_type_info[1].clone();
                    let (closure_fn_ptr, closure_substs_ref) =
                        if let TyKind::Closure(def_id, substs_ref) = closure_adt.get_ty().kind() {
                            (*def_id, substs_ref)
                        } else if let TyKind::FnDef(def_id, substs_ref) =
                            closure_adt.get_ty().kind()
                        {
                            (*def_id, substs_ref)
                        } else {
                            unreachable!();
                        };

                    // Discussion with Dimitar
                    // TODO: report-> we assume there is a finite number of input of a given upperbound on the size
                    // TODO: report-> we assume the elements of the boundedvecs have bounded size
                    // express the complexity of retain as a function of maximum elemts size in the vector

                    // Retrieve the effect of applying the closure once
                    let closure_call_simulation = CalleeInfo {
                        location: None,
                        args_type_info: vec![LocalInfo::new(
                            vector_element_type,
                            transfer_function.tcx,
                            None,
                            transfer_function.fresh_var_id.clone(),
                        )],
                        caller_args_operands: None,
                        destination: None,
                        callee_def_id: closure_fn_ptr,
                        substs_ref: closure_substs_ref,
                    };

                    let mut closure_analysis_result =
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

                    // Update length
                    let bounded_vec_place = callee_info.caller_args_operands.clone().unwrap()[0]
                        .place()
                        .unwrap();
                    assert!(bounded_vec_place.projection.is_empty());
                    transfer_function.state.locals_info[bounded_vec_place.local]
                        .length_of_add_one();

                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }

    mod frame_support_dispatch_specs {
        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
            cost_language::{Cost, CostParameter},
        };

        pub(super) fn try_frame_support_dispatch_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "frame_support::dispatch::UnfilteredDispatchable::dispatch_bypass_filter" => {
                    // We try to get they variable's symbol, otherwise we fallback on the type
                    /*let call_name = transfer_function
                    .state.get_local_info_for_place(&callee_info.args[0].place().unwrap())
                    .get_symbol()
                    .unwrap_or_else(|| callee_info.substs_ref.type_at(0).to_string());*/
                    let call_name = callee_info.substs_ref.type_at(0).to_string();
                    transfer_function
                        .state
                        .add_reads(Cost::Parameter(CostParameter::ReadsOf(call_name.clone())));
                    transfer_function
                        .state
                        .add_writes(Cost::Parameter(CostParameter::WritesOf(call_name.clone())));
                    transfer_function.state.add_events(Cost::Parameter(
                        CostParameter::SizeDepositedOf(call_name.clone()),
                    ));
                    transfer_function
                        .state
                        .add_steps(Cost::Parameter(CostParameter::StepsOf(call_name)));
                    Some((*transfer_function.state).clone())
                }
                "frame_support::dispatch::Dispatchable::dispatch" => {
                    // We try to get they variable's symbol, otherwise we fallback on the type
                    /*let call_name = transfer_function
                    .state.get_local_info_for_place(&callee_info.args[0].place().unwrap())
                    .get_symbol()
                    .unwrap_or_else(|| callee_info.substs_ref.type_at(0).to_string());*/
                    let call_name = callee_info.substs_ref.type_at(0).to_string();
                    transfer_function
                        .state
                        .add_reads(Cost::Parameter(CostParameter::ReadsOf(call_name.clone())));
                    transfer_function
                        .state
                        .add_writes(Cost::Parameter(CostParameter::WritesOf(call_name.clone())));
                    transfer_function.state.add_events(Cost::Parameter(
                        CostParameter::SizeDepositedOf(call_name.clone()),
                    ));
                    transfer_function
                        .state
                        .add_steps(Cost::Parameter(CostParameter::StepsOf(call_name)));
                    Some((*transfer_function.state).clone())
                }
                "frame_support::dispatch::GetDispatchInfo::get_dispatch_info" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }

    mod frame_support_macro_generated_storage_getter_specs {
        use std::collections::HashMap;

        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
            cost_language::{cost_to_big_o, HasSize},
        };

        pub(super) fn try_frame_support_macro_generated_storage_getter<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);

            let path = path.as_str();

            let storage_field_type = callee_info.substs_ref.type_at(0);

            let ty_field_hash_map = transfer_function
                .pallet
                .fields
                .iter()
                .map(|(field_def_id, field)| {
                    (
                        transfer_function.tcx.type_of(field_def_id),
                        (*field).clone(),
                    )
                })
                .fold(HashMap::new(), |mut accum, (field_ty, field)| {
                    accum.insert(field_ty, field);
                    accum
                });

            let storage_field = (*ty_field_hash_map.get(&storage_field_type).unwrap()).clone();

            match path {
                "frame_support::StorageMap::get" => {
                    // call "get" https://paritytech.github.io/substrate/master/src/frame_support/storage/generator/map.rs.html#240
                    transfer_function
                        .state
                        .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));
                    transfer_function
                        .state
                        .add_reads(storage_field.get_size(transfer_function.tcx));
                    Some((*transfer_function.state).clone())
                }
                "frame_support::StorageValue::get" => {
                    // https://paritytech.github.io/substrate/master/frame_support/storage/types/struct.StorageValue.html#method.get
                    // decoding
                    transfer_function
                        .state
                        .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));
                    // storage access
                    transfer_function
                        .state
                        .add_reads(storage_field.get_size(transfer_function.tcx));

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

        pub(super) fn try_frame_support_pallet_prelude_dispatch<'tcx>(
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

            use self::{
                storage_double_map_specs::try_storage_double_map_dispatch,
                storage_map_specs::try_storage_map_dispatch,
                storage_value_specs::try_storage_value_dispatch,
            };

            pub(super) fn try_storage_dispatch<'tcx>(
                transfer_function: &mut TransferFunction<'tcx, '_, '_>,
                callee_info: &CalleeInfo<'tcx>,
                storage_field: Field,
            ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
                let path = transfer_function
                    .tcx
                    .def_path_str(callee_info.callee_def_id);

                if path.starts_with("frame_support::pallet_prelude::StorageDoubleMap::") {
                    try_storage_double_map_dispatch(transfer_function, callee_info, storage_field)
                } else if path.starts_with("frame_support::pallet_prelude::StorageMap::") {
                    try_storage_map_dispatch(transfer_function, callee_info, storage_field)
                } else if path.starts_with("frame_support::pallet_prelude::StorageValue::") {
                    try_storage_value_dispatch(transfer_function, callee_info, storage_field)
                } else {
                    None
                }
            }

            mod storage_double_map_specs {
                use crate::analysis::{
                    cost_analysis::{CalleeInfo, TransferFunction},
                    cost_domain::ExtendedCostAnalysisDomain,
                    cost_language::{cost_to_big_o, Cost, HasSize},
                    pallet::Field,
                };

                pub(super) fn try_storage_double_map_dispatch<'tcx>(
                    transfer_function: &mut TransferFunction<'tcx, '_, '_>,
                    callee_info: &CalleeInfo<'tcx>,
                    storage_field: Field,
                ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
                    let path = transfer_function
                        .tcx
                        .def_path_str(callee_info.callee_def_id);
                    let path = path.as_str();

                    match path {
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::append" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::contains_key" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::decode_len" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::drain" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::drain_prefix" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::get" => {
                            // call "get" https://paritytech.github.io/substrate/master/src/frame_support/storage/generator/map.rs.html#240
                            transfer_function
                            .state
                            .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));
                            transfer_function
                                .state
                                .add_reads(storage_field.get_size(transfer_function.tcx));
                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::insert" => {
                            // call "insert" https://paritytech.github.io/substrate/master/src/frame_support/storage/generator/map.rs.html#248
                            transfer_function
                            .state
                            .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));
                            transfer_function
                                .state
                                .add_writes(storage_field.get_size(transfer_function.tcx));
                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::iter" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::iter_from" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::iter_key_prefix" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::iter_key_prefix_from" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::iter_keys" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::iter_keys_from" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::iter_prefix" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::iter_prefix_from" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::iter_prefix_values" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::iter_values" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::migrate_keys" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::mutate" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::mutate_exists" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::remove" => {
                            // call "remove" https://paritytech.github.io/substrate/master/src/frame_support/storage/generator/map.rs.html#248
                            transfer_function.state.add_step();
                            // Write "None" to storage
                            transfer_function.state.add_writes(Cost::Scalar(1));

                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::remove_all" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::remove_prefix" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::swap" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::take" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::translate" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::translate_values" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::try_append" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::try_get" => {
                            // call "try_get" https://paritytech.github.io/substrate/master/src/frame_support/storage/generator/map.rs.html#244
                            transfer_function
                            .state
                            .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));
                            transfer_function
                                .state
                                .add_reads(storage_field.get_size(transfer_function.tcx));

                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::try_mutate" => {todo!()}
                        "frame_support::pallet_prelude::StorageDoubleMap::<Prefix, Hasher1, Key1, Hasher2, Key2, Value, QueryKind, OnEmpty, MaxValues>::try_mutate_exists" => {todo!()}
                        _ => None,
                    }
                }
            }

            mod storage_map_specs {
                use crate::analysis::{
                    cost_analysis::{CalleeInfo, TransferFunction},
                    cost_domain::ExtendedCostAnalysisDomain,
                    cost_language::{cost_to_big_o, Cost, HasSize},
                    pallet::Field,
                };

                use rustc_middle::ty::TyKind;

                pub(super) fn try_storage_map_dispatch<'tcx>(
                    transfer_function: &mut TransferFunction<'tcx, '_, '_>,
                    callee_info: &CalleeInfo<'tcx>,
                    storage_field: Field,
                ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
                    let path = transfer_function
                        .tcx
                        .def_path_str(callee_info.callee_def_id);
                    let path = path.as_str();

                    match path {
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::append" => {todo!()}
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::contains_key" => {
                            // call "contains_key" https://paritytech.github.io/substrate/master/src/frame_support/storage/generator/map.rs.html#236
                            transfer_function.state.add_step();
                            transfer_function
                                .state
                                .add_reads(storage_field.get_size(transfer_function.tcx));
                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::decode_len" => {todo!()}
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::drain" => {todo!()}
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::get" => {
                            // call "get" https://paritytech.github.io/substrate/master/src/frame_support/storage/generator/map.rs.html#240
                            transfer_function
                            .state
                            .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));
                            transfer_function
                                .state
                                .add_reads(storage_field.get_size(transfer_function.tcx));
                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::insert" => {
                            // call "insert" https://paritytech.github.io/substrate/master/src/frame_support/storage/generator/map.rs.html#248
                            transfer_function
                            .state
                            .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));
                            transfer_function
                                .state
                                .add_writes(storage_field.get_size(transfer_function.tcx));
                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::iter" => {todo!()}
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::iter_from" => {todo!()}
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::iter_keys" => {todo!()}
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::iter_key_from" => {todo!()}
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::iter_values" => {todo!()}
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::migrate_key" => {todo!()}
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::mutate" => {
                            // call "mutate" https://paritytech.github.io/substrate/master/src/frame_support/storage/generator/map.rs.html#256
                            transfer_function
                            .state
                            .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));
                            transfer_function
                                .state
                                .add_reads(storage_field.get_size(transfer_function.tcx));

                            // Account for closure call
                            let closure_adt = callee_info.args_type_info[1].clone();
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
                                caller_args_operands: None,
                                destination: None,
                                callee_def_id: closure_fn_ptr,
                                substs_ref: closure_substs_ref,
                            };

                            transfer_function.fn_call_analysis(closure_call_simulation, false);

                            transfer_function
                                .state
                                .add_writes(storage_field.get_size(transfer_function.tcx));

                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::mutate_exists" => {todo!()}
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::remove" => {
                            // call "remove" https://paritytech.github.io/substrate/master/src/frame_support/storage/generator/map.rs.html#248
                            transfer_function.state.add_step();
                            // Write "None" to storage
                            transfer_function.state.add_writes(Cost::Scalar(1));

                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::remove_all" => {todo!()}
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::swap" => {todo!()}
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::take" => {
                            // call "take" https://paritytech.github.io/substrate/master/src/frame_support/storage/generator/map.rs.html#303
                            transfer_function
                            .state
                            .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));

                            transfer_function
                                .state
                                .add_reads(storage_field.get_size(transfer_function.tcx));

                            // Write "None" to storage
                            transfer_function.state.add_writes(Cost::Scalar(1));

                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::translate" => {todo!()}
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::translate_values" => {todo!()}
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::try_append" => {todo!()}
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::try_get" => {
                            // call "try_get" https://paritytech.github.io/substrate/master/src/frame_support/storage/generator/map.rs.html#244
                            transfer_function
                            .state
                            .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));
                            transfer_function
                                .state
                                .add_reads(storage_field.get_size(transfer_function.tcx));

                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::try_mutate" => {
                            // call "mutate" https://paritytech.github.io/substrate/master/src/frame_support/storage/generator/map.rs.html#269
                            transfer_function
                            .state
                            .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));
                            transfer_function
                                .state
                                .add_reads(storage_field.get_size(transfer_function.tcx));

                            // Account for closure call
                            let closure_adt = callee_info.args_type_info[1].clone();
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
                                caller_args_operands: None,
                                destination: None,
                                callee_def_id: closure_fn_ptr,
                                substs_ref: closure_substs_ref,
                            };

                            transfer_function.fn_call_analysis(closure_call_simulation, false);

                            transfer_function
                                .state
                                .add_writes(storage_field.get_size(transfer_function.tcx));
                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageMap::<Prefix, Hasher, Key, Value, QueryKind, OnEmpty, MaxValues>::exists" => {todo!()}
                        _ => None,
                    }
                }
            }

            mod storage_value_specs {
                use crate::analysis::{
                    cost_analysis::{CalleeInfo, TransferFunction},
                    cost_domain::ExtendedCostAnalysisDomain,
                    cost_language::{cost_to_big_o, Cost, HasSize},
                    pallet::Field,
                };

                use rustc_middle::ty::TyKind;

                pub(super) fn try_storage_value_dispatch<'tcx>(
                    transfer_function: &mut TransferFunction<'tcx, '_, '_>,
                    callee_info: &CalleeInfo<'tcx>,
                    storage_field: Field,
                ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
                    let path = transfer_function
                        .tcx
                        .def_path_str(callee_info.callee_def_id);
                    let path = path.as_str();

                    match path {
                        "frame_support::pallet_prelude::StorageValue::<Prefix, Value, QueryKind, OnEmpty>::append" => {todo!()}
                        "frame_support::pallet_prelude::StorageValue::<Prefix, Value, QueryKind, OnEmpty>::decode_len" => {todo!()}
                        "frame_support::pallet_prelude::StorageValue::<Prefix, Value, QueryKind, OnEmpty>::exists" => {
                            // https://paritytech.github.io/substrate/master/frame_support/storage/types/struct.StorageValue.html#method.exists
                            transfer_function.state.add_step();
                            // storage access
                            transfer_function
                                .state
                                .add_reads(storage_field.get_size(transfer_function.tcx));

                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageValue::<Prefix, Value, QueryKind, OnEmpty>::get" => {
                            // https://paritytech.github.io/substrate/master/frame_support/storage/types/struct.StorageValue.html#method.get
                            // decoding
                            transfer_function
                            .state
                            .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));
                            // storage access
                            transfer_function
                                .state
                                .add_reads(storage_field.get_size(transfer_function.tcx));

                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageValue::<Prefix, Value, QueryKind, OnEmpty>::kill" => {
                            // https://paritytech.github.io/substrate/master/frame_support/storage/types/struct.StorageValue.html#method.kill
                            transfer_function.state.add_step();
                            // Write None to database
                            transfer_function.state.add_writes(Cost::Scalar(1));

                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageValue::<Prefix, Value, QueryKind, OnEmpty>::mutate" => {
                            // https://paritytech.github.io/substrate/master/frame_support/storage/types/struct.StorageValue.html#method.mutate

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
                                caller_args_operands: None,
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
                        "frame_support::pallet_prelude::StorageValue::<Prefix, Value, QueryKind, OnEmpty>::put" => {
                            // https://paritytech.github.io/substrate/master/frame_support/storage/types/struct.StorageValue.html#method.put
                            transfer_function
                            .state
                            .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));
                            // storage access
                            transfer_function
                                .state
                                .add_writes(storage_field.get_size(transfer_function.tcx));
                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageValue::<Prefix, Value, QueryKind, OnEmpty>::set" => {
                            // https://paritytech.github.io/substrate/master/frame_support/storage/types/struct.StorageValue.html#method.set
                            // encoding
                            transfer_function
                            .state
                            .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));
                            // storage access
                            transfer_function
                                .state
                                .add_writes(storage_field.get_size(transfer_function.tcx));

                            Some((*transfer_function.state).clone())
                        }
                        "frame_support::pallet_prelude::StorageValue::<Prefix, Value, QueryKind, OnEmpty>::take" => {todo!()}
                        "frame_support::pallet_prelude::StorageValue::<Prefix, Value, QueryKind, OnEmpty>::translate" => {todo!()}
                        "frame_support::pallet_prelude::StorageValue::<Prefix, Value, QueryKind, OnEmpty>::try_append" => {todo!()}
                        "frame_support::pallet_prelude::StorageValue::<Prefix, Value, QueryKind, OnEmpty>::try_get" => {
                            // https://paritytech.github.io/substrate/master/frame_support/storage/types/struct.StorageValue.html#method.try_get
                            transfer_function
                            .state
                            .add_steps(cost_to_big_o(storage_field.get_size(transfer_function.tcx)));
                            transfer_function
                                .state
                                .add_reads(storage_field.get_size(transfer_function.tcx));
                            Some((*transfer_function.state).clone())
                        }
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
                                caller_args_operands: None,
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
            cost_language::{cost_to_big_o, Cost, CostParameter, HasSize},
            types::Type,
        };

        pub(super) fn try_frame_support_traits_dispatch<'tcx>(
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
                "frame_support::traits::WrapperKeepOpaque::<T>::encoded" => {
                    let underlying = callee_info.args_type_info[0].clone();
                    transfer_function.state.locals_info[callee_info.destination.unwrap().local]
                        .set_local_info(underlying);
                        transfer_function.state.locals_info[callee_info.destination.unwrap().local].fill_with_inner_size(transfer_function.tcx);

                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "frame_support::traits::WrapperKeepOpaque::<T>::encoded_len" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "frame_support::traits::WrapperKeepOpaque::<T>::try_decode" => {
                    transfer_function
                        .state
                        .add_steps(Cost::BigO(Box::new(Cost::Parameter(
                            CostParameter::ValueOf(format!(
                                "{:?}::get()",
                                callee_info.args_type_info[0].get_ty()
                            )),
                        ))));
                    Some((*transfer_function.state).clone())
                }
                "frame_support::traits::ReservableCurrency::can_reserve"
                | "frame_support::traits::ReservableCurrency::slash_reserved"
                | "frame_support::traits::ReservableCurrency::reserved_balance"
                | "frame_support::traits::ReservableCurrency::reserve"
                | "frame_support::traits::ReservableCurrency::unreserve"
                | "frame_support::traits::ReservableCurrency::repatriate_reserved"
                | "frame_support::traits::Currency::transfer"
                | "frame_support::traits::fungible::Inspect::reducible_balance"
                | "frame_support::traits::LockableCurrency::remove_lock"
                | "frame_support::traits::LockableCurrency::set_lock" => {
                    let fn_name = path.split("::").last().unwrap();
                    transfer_function
                        .state
                        .add_reads(Cost::Parameter(CostParameter::ReadsOf(format!(
                            "Currency::{}",
                            fn_name
                        ))));
                    transfer_function
                        .state
                        .add_writes(Cost::Parameter(CostParameter::WritesOf(format!(
                            "Currency::{}",
                            fn_name
                        ))));
                    transfer_function.state.add_events(Cost::Parameter(
                        CostParameter::SizeDepositedOf(format!("Currency::{}", fn_name)),
                    ));
                    transfer_function
                        .state
                        .add_steps(Cost::Parameter(CostParameter::StepsOf(format!(
                            "Currency::{}",
                            fn_name
                        ))));
                    Some((*transfer_function.state).clone())
                }
                "frame_support::traits::StoredMap::try_mutate_exists" => {
                    let value_type_size =
                        Type::from_mir_ty(transfer_function.tcx, callee_info.substs_ref.type_at(2));

                    transfer_function
                        .state
                        .add_reads(value_type_size.get_size(transfer_function.tcx));
                    transfer_function
                        .state
                        .add_writes(value_type_size.get_size(transfer_function.tcx));
                    transfer_function.state.add_steps(cost_to_big_o(
                        value_type_size.get_size(transfer_function.tcx),
                    ));
                    Some((*transfer_function.state).clone())
                }
                "frame_support::traits::VestingSchedule::can_add_vesting_schedule"
                | "frame_support::traits::VestingSchedule::add_vesting_schedule" => {
                    let fn_name = path.split("::").last().unwrap();
                    transfer_function
                        .state
                        .add_reads(Cost::Parameter(CostParameter::ReadsOf(format!(
                            "VestingSchedule::{}",
                            fn_name
                        ))));
                    transfer_function
                        .state
                        .add_writes(Cost::Parameter(CostParameter::WritesOf(format!(
                            "VestingSchedule::{}",
                            fn_name
                        ))));
                    transfer_function.state.add_events(Cost::Parameter(
                        CostParameter::SizeDepositedOf(format!("VestingSchedule::{}", fn_name)),
                    ));
                    transfer_function
                        .state
                        .add_steps(Cost::Parameter(CostParameter::StepsOf(format!(
                            "VestingSchedule::{}",
                            fn_name
                        ))));
                    Some((*transfer_function.state).clone())
                }
                "frame_support::traits::OriginTrait::set_caller_from" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }
}

pub(super) mod frame_system_specs {
    use regex::Regex;

    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_domain::ExtendedCostAnalysisDomain,
    };

    pub(super) fn try_frame_system_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: &CalleeInfo<'tcx>,
    ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "frame_system::ensure_signed" | "frame_system::ensure_root" => {
                transfer_function.state.add_step();
                Some((*transfer_function.state).clone())
            }
            _ => {
                let block_number_regex =
                    Regex::new(r"frame_system::Pallet::<.*\s*(,.*)*>::block_number").unwrap();

                let extrinsic_index_regex = Regex::new(
                    r"frame_system::<impl frame_system::Pallet<.*\s*(,.*)*>>::extrinsic_index",
                )
                .unwrap();

                if block_number_regex.is_match(path) | extrinsic_index_regex.is_match(path) {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                } else {
                    unimplemented!("{}", path)
                }
            }
        }
    }
}

pub(super) mod pallet_specs {
    use regex::Regex;

    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_domain::ExtendedCostAnalysisDomain,
        cost_language::{Cost, CostParameter},
        events_variants_domain::Variants,
    };
    use rustc_middle::ty::TyKind;

    pub(super) fn try_pallet_dispatch<'tcx>(
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

pub(super) mod parity_scale_codec_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_domain::ExtendedCostAnalysisDomain,
        cost_language::cost_to_big_o,
    };

    use rustc_middle::ty::TyKind;

    pub(super) fn try_parity_scale_codec_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: &CalleeInfo<'tcx>,
    ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "parity_scale_codec::Encode::using_encoded" => {
                // Account for closure call
                let closure_adt = callee_info.args_type_info[1].clone();
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
                    args_type_info: vec![callee_info.args_type_info[0].clone()],
                    caller_args_operands: None,
                    destination: callee_info.destination,
                    callee_def_id: closure_fn_ptr,
                    substs_ref: closure_substs_ref,
                };

                transfer_function.fn_call_analysis(closure_call_simulation, false);
                Some((*transfer_function.state).clone())
            }
            "parity_scale_codec::Decode::decode" => {
                // Ideally should be parametrized on the length of the vector to decode,
                // but we can assume that is proportional to the type to be decoded to
                transfer_function.state.add_steps(cost_to_big_o(
                    callee_info.args_type_info[0].get_size(transfer_function.tcx),
                ));
                Some((*transfer_function.state).clone())
            }
            _ => unimplemented!("{}", path),
        }
    }
}

pub(super) mod sp_io_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_domain::ExtendedCostAnalysisDomain,
        cost_language::cost_to_big_o,
    };

    pub(super) fn try_sp_io_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: &CalleeInfo<'tcx>,
    ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);
        let path = path.as_str();
        match path {
            "sp_io::hashing::blake2_256" => {
                transfer_function.state.add_steps(cost_to_big_o(
                    callee_info.args_type_info[0].get_size(transfer_function.tcx),
                ));

                Some((*transfer_function.state).clone())
            }
            _ => None,
        }
    }
}

pub(super) mod sp_runtime_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_domain::ExtendedCostAnalysisDomain,
    };

    use self::sp_runtime_traits_specs::try_sp_runtime_traits_dispatch;

    pub(super) fn try_sp_runtime_dispatch<'tcx>(
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
            cost_language::{Cost, CostParameter},
        };

        use rustc_middle::ty::TyKind;

        pub(super) fn try_sp_runtime_traits_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);

            let path = path.as_str();

            match path {
                "sp_runtime::traits::CheckedMul::checked_mul" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "sp_runtime::traits::Convert::convert" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "sp_runtime::traits::One::one" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "sp_runtime::traits::StaticLookup::lookup" => {
                    transfer_function.state.add_step();

                    // get the destination type to know what type is read from storage
                    // return type is Result<read_type, error_type>
                    let res = transfer_function
                        .state
                        .get_local_info_for_place(&callee_info.destination.unwrap())
                        .unwrap()
                        .get_ty()
                        .kind();
                    let read_type = match res {
                        TyKind::Adt(_, substs_ref) => substs_ref.type_at(0),
                        _ => unreachable!(),
                    };
                    transfer_function
                        .state
                        .add_reads(Cost::Parameter(CostParameter::SizeOf(
                            read_type.to_string(),
                        )));
                    Some((*transfer_function.state).clone())
                }
                "sp_runtime::traits::StaticLookup::unlookup" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "sp_runtime::traits::Saturating::saturating_add"
                | "sp_runtime::traits::Saturating::saturating_mul"
                | "sp_runtime::traits::Saturating::saturating_sub" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "sp_runtime::traits::TrailingZeroInput::<'a>::new" => {
                    // https://paritytech.github.io/substrate/master/sp_runtime/traits/struct.TrailingZeroInput.html#method.new

                    let underlying = callee_info.args_type_info[0].clone();
                    transfer_function.state.locals_info[callee_info.destination.unwrap().local]
                        .set_local_info(underlying);
                        transfer_function.state.locals_info[callee_info.destination.unwrap().local].fill_with_inner_size(transfer_function.tcx);

                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "sp_runtime::traits::Zero::zero" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "sp_runtime::traits::Zero::is_zero" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }
}

pub(super) mod std_specs {
    use crate::analysis::{
        cost_analysis::{CalleeInfo, TransferFunction},
        cost_domain::ExtendedCostAnalysisDomain,
    };

    use self::{
        std_alloc_specs::try_std_alloc_dispatch, std_clone_specs::try_std_clone_dispatch,
        std_cmp_specs::try_std_cmp_dispatch, std_convert_specs::try_std_convert_dispatch,
        std_default_specs::try_std_default_dispatch,
        std_intrinsics_specs::try_std_intrinsics_dispatch, std_iter_specs::try_std_iter_dispatch,
        std_ops_specs::try_std_ops_dispatch, std_result_specs::try_std_resut_dispatch,
        std_slice_specs::try_std_slice_dispatch, std_vec_specs::try_std_vec_dispatch,
    };

    pub(super) fn try_std_dispatch<'tcx>(
        transfer_function: &mut TransferFunction<'tcx, '_, '_>,
        callee_info: &CalleeInfo<'tcx>,
    ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
        let path = transfer_function
            .tcx
            .def_path_str(callee_info.callee_def_id);

        if path.starts_with("std::alloc::") {
            try_std_alloc_dispatch(transfer_function, callee_info)
        } else if path.starts_with("std::clone::") {
            try_std_clone_dispatch(transfer_function, callee_info)
        } else if path.starts_with("std::cmp::") {
            try_std_cmp_dispatch(transfer_function, callee_info)
        } else if path.starts_with("std::convert::") {
            try_std_convert_dispatch(transfer_function, callee_info)
        } else if path.starts_with("std::default::") {
            try_std_default_dispatch(transfer_function, callee_info)
        } else if path.starts_with("std::intrinsics::") {
            try_std_intrinsics_dispatch(transfer_function, callee_info)
        } else if path.starts_with("std::iter") {
            try_std_iter_dispatch(transfer_function, callee_info)
        } else if path.starts_with("std::ops::") {
            try_std_ops_dispatch(transfer_function, callee_info)
        } else if path.starts_with("std::result::") {
            try_std_resut_dispatch(transfer_function, callee_info)
        } else if path.starts_with("std::slice::") {
            try_std_slice_dispatch(transfer_function, callee_info)
        } else if path.starts_with("std::vec::") {
            try_std_vec_dispatch(transfer_function, callee_info)
        } else {
            None
        }
    }

    mod std_alloc_specs {
        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
        };

        pub(super) fn try_std_alloc_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();
            match path {
                "std::alloc::Allocator::allocate" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::alloc::Allocator::deallocate" => {
                    // deallocate boils down to libc::free
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::alloc::Allocator::exchange_malloc" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::alloc::handle_alloc_error" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }

    mod std_clone_specs {
        use std::{cell::RefCell, rc::Rc};

        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
            cost_language::Cost,
        };

        pub(super) fn try_std_clone_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "std::clone::Clone::clone" => {
                    // Clone the inner value of the lentgh if there is one
                    let cloned = callee_info.args_type_info[0].clone();
                    let length_of_cloned: Option<Cost> = cloned.length_of.borrow().clone();

                    if let Some(_) = length_of_cloned {
                        transfer_function.state.locals_info
                            [callee_info.destination.unwrap().local]
                            .length_of = Rc::new(RefCell::new(length_of_cloned));
                    }

                    // Soundness inconsistency here, we would need Instance to
                    // resolve to the concrete implementation
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }

    mod std_cmp_specs {
        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
        };

        pub(super) fn try_std_cmp_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "std::cmp::Ord::cmp" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::cmp::PartialEq::eq" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::cmp::PartialOrd::partial_cmp" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }

    mod std_convert_specs {
        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
        };

        pub(super) fn try_std_convert_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "std::convert::AsRef::as_ref" => {
                    let underlying = callee_info.args_type_info[0].clone();
                    transfer_function.state.locals_info[callee_info.destination.unwrap().local]
                        .set_local_info(underlying);
                        transfer_function.state.locals_info[callee_info.destination.unwrap().local].fill_with_inner_size(transfer_function.tcx);

                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::convert::From::from" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::convert::Into::into" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::convert::TryInto::try_into" => {
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

        pub(super) fn try_std_default_dispatch<'tcx>(
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

    mod std_intrinsics_specs {
        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
        };

        pub(super) fn try_std_intrinsics_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "std::intrinsics::add_with_overflow" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::intrinsics::assert_inhabited" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::intrinsics::assume" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::intrinsics::min_align_of_val" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::intrinsics::mul_with_overflow" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::intrinsics::offset" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::intrinsics::ptr_guaranteed_eq" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::intrinsics::saturating_add" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::intrinsics::size_of_val" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::intrinsics::transmute" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::intrinsics::unlikely" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }

    mod std_iter_specs {
        use crate::analysis::{
            cost_analysis::{AnalysisState, CalleeInfo, TransferFunction},
            cost_domain::{ExtendedCostAnalysisDomain, LocalInfo},
            cost_language::cost_to_big_o,
        };

        use rustc_middle::ty::TyKind;

        pub(super) fn try_std_iter_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "std::iter::IntoIterator::into_iter" => {
                    // Keep underlying type info instead of IntoIter
                    let underlying = callee_info.args_type_info[0].clone();
                    transfer_function.state.locals_info[callee_info.destination.unwrap().local]
                        .set_local_info(underlying.clone());

                    transfer_function
                        .state
                        .forward_symbolic_attributes(&callee_info.destination.unwrap(), underlying);
                        transfer_function.state.locals_info[callee_info.destination.unwrap().local].fill_with_inner_size(transfer_function.tcx);

                    // Iterator is managing a pointer to the vec/array/whatever
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::iter::Iterator::collect" => {
                    // Account for iterator complexity (O(n))
                    transfer_function.state.forward_symbolic_attributes(
                        &callee_info.destination.unwrap(),
                        callee_info.args_type_info[0].clone(),
                    );
                    transfer_function.state.add_steps(cost_to_big_o(
                        callee_info.args_type_info[0].get_size(transfer_function.tcx),
                    ));
                    Some((*transfer_function.state).clone())
                }
                "std::iter::Iterator::enumerate" => {
                    // Account for iterator complexity (O(n))
                    transfer_function.state.forward_symbolic_attributes(
                        &callee_info.destination.unwrap(),
                        callee_info.args_type_info[0].clone(),
                    );
                    transfer_function.state.add_steps(cost_to_big_o(
                        callee_info.args_type_info[0].get_size(transfer_function.tcx),
                    ));
                    Some((*transfer_function.state).clone())
                }
                "std::iter::Iterator::filter" | "std::iter::Iterator::filter_map" => {
                    // Account for closure
                    let vector_element_type = if let TyKind::Adt(_, substs_ref) =
                        callee_info.args_type_info[0].get_ty().kind()
                    {
                        substs_ref.type_at(0)
                    } else if let TyKind::Opaque(def_id, _) = callee_info.args_type_info[0].get_ty().kind() && let TyKind::Adt(_, substs_ref) = transfer_function.tcx.type_of(def_id).kind() && let TyKind::Closure(_, substs_ref) = substs_ref.type_at(1).kind() {
                        substs_ref.as_closure().sig().input(0).skip_binder()
                    } else {
                        unreachable!("{:?}", callee_info.args_type_info[0].get_ty().kind())
                    };

                    let closure_adt = callee_info.args_type_info[1].clone();
                    let (closure_fn_ptr, closure_substs_ref) =
                        if let TyKind::Closure(def_id, substs_ref) = closure_adt.get_ty().kind() {
                            (*def_id, substs_ref)
                        } else if let TyKind::FnDef(def_id, substs_ref) =
                            closure_adt.get_ty().kind()
                        {
                            (*def_id, substs_ref)
                        } else {
                            unreachable!();
                        };

                    // Account for closure call
                    let closure_call_simulation = CalleeInfo {
                        location: None,
                        args_type_info: vec![LocalInfo::new(
                            vector_element_type,
                            transfer_function.tcx,
                            None,
                            transfer_function.fresh_var_id.clone(),
                        )],
                        caller_args_operands: None,
                        destination: callee_info.destination,
                        callee_def_id: closure_fn_ptr,
                        substs_ref: closure_substs_ref,
                    };

                    let mut closure_analysis_result =
                        transfer_function.fn_call_analysis(closure_call_simulation, true);

                    // Get the size of the iteratable
                    let vec_big_o_size = cost_to_big_o(
                        callee_info.args_type_info[0].get_size(transfer_function.tcx),
                    );

                    // Then multiply it by complexity
                    closure_analysis_result.cost_big_o_mul(vec_big_o_size);

                    transfer_function.state.forward_symbolic_attributes(
                        &callee_info.destination.unwrap(),
                        callee_info.args_type_info[0].clone(),
                    );

                    transfer_function.state.inter_join(&closure_analysis_result);
                    Some((*transfer_function.state).clone())
                }
                _ => {
                    *transfer_function.analysis_success_state.borrow_mut() = AnalysisState::Failure;
                    println!("Iterators not supported yet");
                    Some((*transfer_function.state).clone())
                }
            }
        }
    }
    mod std_ops_specs {
        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
        };

        pub(super) fn try_std_ops_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "std::ops::Add::add" => {
                    // Soundness inconsistency here, we would need Instance to
                    // resolve to the concrete implementation
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::ops::BitOr::bitor" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::ops::Deref::deref" => {
                    // Keep type with more information when derefencing
                    let underlying = callee_info.args_type_info[0].clone();
                    transfer_function.state.locals_info[callee_info.destination.unwrap().local]
                        .set_local_info(underlying.get_member(0).unwrap().clone());
                    
                    transfer_function.state.locals_info[callee_info.destination.unwrap().local].fill_with_inner_size(transfer_function.tcx);

                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::ops::Div::div" => {
                    // Soundness inconsistency here, we would need Instance to
                    // resolve to the concrete implementation
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
                "std::ops::Index::index" | "std::ops::IndexMut::index_mut" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::ops::Mul::mul" => {
                    // Soundness inconsistency here, we would need Instance to
                    // resolve to the concrete implementation
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::ops::Rem::rem" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::ops::Sub::sub" | "std::ops::SubAssign::sub_assign" => {
                    // Soundness inconsistency here, we would need Instance to
                    // resolve to the concrete implementation
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::ops::Try::branch" => {

                    let branched_ty = callee_info.args_type_info[0].get_ty();

                    if branched_ty.is_adt() && let Some(adt_def) = branched_ty.ty_adt_def() &&  transfer_function.tcx.def_path_str(adt_def.did()) == "std::result::Result" {
                        // Replace the local info of ControlFlow::Continue branch with the one of Result::Ok
                        assert!(callee_info.destination.unwrap().projection.is_empty());
                        transfer_function.state.locals_info[callee_info.destination.unwrap().local].set_sub_member(vec![0,0], callee_info.args_type_info[0].get_member(0).unwrap().clone());
                    }

                    transfer_function.state.add_step();

                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }

    mod std_result_specs {
        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
        };

        pub(super) fn try_std_resut_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "std::result::unwrap_failed" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }

    mod std_slice_specs {
        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
            cost_language::cost_to_big_o,
        };

        pub(super) fn try_std_slice_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "std::slice::SliceIndex::get" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::slice::<impl [T]>::to_vec" => {
                    // Keep type with more information when converting to vec
                    let source_ty = callee_info.args_type_info[0].clone();
                    transfer_function.state.locals_info[callee_info.destination.unwrap().local] =
                        source_ty;

                    transfer_function.state.add_steps(cost_to_big_o(
                        callee_info.args_type_info[0].get_size(transfer_function.tcx),
                    ));

                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }

    mod std_vec_specs {
        use std::{cell::RefCell, rc::Rc};

        use crate::analysis::{
            cost_analysis::{CalleeInfo, TransferFunction},
            cost_domain::ExtendedCostAnalysisDomain,
            cost_language::Cost,
        };

        pub(super) fn try_std_vec_dispatch<'tcx>(
            transfer_function: &mut TransferFunction<'tcx, '_, '_>,
            callee_info: &CalleeInfo<'tcx>,
        ) -> Option<ExtendedCostAnalysisDomain<'tcx>> {
            let path = transfer_function
                .tcx
                .def_path_str(callee_info.callee_def_id);
            let path = path.as_str();

            match path {
                "std::vec::Vec::<T, A>::insert" => {
                    let vec = callee_info.args_type_info[0].clone();

                    // Maybe grows allocated memory region
                    let steps_of_insert =
                        Cost::BigO(Box::new((vec.length_of.borrow().clone().unwrap()).clone()));
                    transfer_function.state.add_steps(steps_of_insert);

                    // Update length
                    let vec_place = callee_info.caller_args_operands.clone().unwrap()[0]
                        .place()
                        .unwrap();
                    assert!(vec_place.projection.is_empty());
                    transfer_function.state.locals_info[vec_place.local].length_of_add_one();
                    transfer_function.state.locals_info[vec_place.local]
                        .fill_with_inner_size(transfer_function.tcx);

                    Some((*transfer_function.state).clone())
                }
                "std::vec::Vec::<T, A>::insert::assert_failed" => {
                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::vec::Vec::<T>::new" => {
                    assert!(callee_info.destination.unwrap().projection.is_empty());
                    transfer_function.state.locals_info[callee_info.destination.unwrap().local]
                        .length_of = Rc::new(RefCell::new(Some(Cost::default())));
                    transfer_function.state.locals_info[callee_info.destination.unwrap().local]
                        .fill_with_inner_size(transfer_function.tcx);

                    transfer_function.state.add_step();
                    Some((*transfer_function.state).clone())
                }
                "std::vec::Vec::<T, A>::push" => {
                    let vec_place = callee_info.caller_args_operands.clone().unwrap()[0]
                        .place()
                        .unwrap();

                    // Maybe grows allocated memory region
                    let steps_of_push = Cost::BigO(Box::new(
                        (transfer_function.state.locals_info[vec_place.local]
                            .length_of
                            .borrow()
                            .clone()
                            .unwrap())
                        .clone(),
                    ));
                    transfer_function.state.add_steps(steps_of_push);

                    // Update length
                    assert!(vec_place.projection.is_empty());
                    transfer_function.state.locals_info[vec_place.local].length_of_add_one();
                    transfer_function.state.locals_info[vec_place.local]
                        .fill_with_inner_size(transfer_function.tcx);
                    Some((*transfer_function.state).clone())
                }
                _ => None,
            }
        }
    }
}
