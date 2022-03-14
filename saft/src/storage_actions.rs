use rustc_hir::def_id::DefId;
use rustc_middle::ty::TyCtxt;
// https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html
// https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageMap.html

use crate::{
    analysis_utils::{get_fn_name, get_fn_name_with_path},
    mir_visitor::Context,
    weights::Weights,
};

pub enum StorageValueActions {
    Exists,
    Get,
    TryGet,
    Translate,
    Put,
    Set,
    Mutate,
    TryMutate,
    Kill,
    Take,
    Append,
    DecodeLen,
    TryAppend,
}

impl StorageValueActions {
    pub fn is_storage_value_action(action: &str) -> bool {
        StorageValueActions::storage_value_actions().contains(&action.to_owned())
    }

    pub fn storage_value_actions() -> Vec<String> {
        let storage_value_actions = vec![
            "exists",
            "get",
            "try_get",
            "translate",
            "put",
            "set",
            "mutate",
            "try_mutate",
            "kill",
            "take",
            "append",
            "decode_len",
            "try_append",
        ];

        let storage_value_actions_full = storage_value_actions
            .iter()
            .map(|e| "frame_support::StorageValue::".to_owned() + e)
            .collect::<Vec<_>>();
        storage_value_actions_full
    }

    pub fn from(action: &str) -> StorageValueActions {
        match action {
            "exists" => StorageValueActions::Exists,
            "get" => StorageValueActions::Get,
            "try_get" => StorageValueActions::TryGet,
            "translate" => StorageValueActions::Translate,
            "put" => StorageValueActions::Put,
            "set" => StorageValueActions::Set,
            "mutate" => StorageValueActions::Mutate,
            "try_mutate" => StorageValueActions::TryMutate,
            "kill" => StorageValueActions::Kill,
            "take" => StorageValueActions::Take,
            "append" => StorageValueActions::Append,
            "decode_len" => StorageValueActions::DecodeLen,
            "try_append" => StorageValueActions::TryAppend,
            _ => panic!("Invalid StorageValue action"),
        }
    }

    pub fn get_weights(&self) -> Weights {
        let default = Weights::default();
        match self {
            StorageValueActions::Exists => Weights::new(1, 0),
            StorageValueActions::Get => Weights::new(1, 0),
            StorageValueActions::TryGet => Weights::new(1, 0),
            StorageValueActions::Translate => default,
            StorageValueActions::Put => Weights::new(0, 1),
            StorageValueActions::Set => Weights::new(0, 1),
            StorageValueActions::Mutate => Weights::new(1, 1),
            StorageValueActions::TryMutate => Weights::new(1, 1),
            StorageValueActions::Kill => default,
            StorageValueActions::Take => default,
            StorageValueActions::Append => default,
            StorageValueActions::DecodeLen => default,
            StorageValueActions::TryAppend => default,
        }
    }
}

pub enum StorageMapActions {
    ContainsKey,
    Get,
    TryGet,
    Swap,
    Insert,
    Remove,
    Mutate,
    TryMutate,
    MutateExists,
    TryMutateExists,
    Take,
    Append,
    DecodeLen,
    MigrateKey,
    RemoveAll,
    IterValues,
    TranslateValues,
    TryAppend,
    Iter,
    IterFrom,
    IterKeys,
    IterKeysFrom,
    Drain,
    Translate,
}

impl StorageMapActions {
    pub fn is_storage_map_action(action: &str) -> bool {
        StorageMapActions::storage_map_actions().contains(&action.to_owned())
    }

    pub fn storage_map_actions() -> Vec<String> {
        let storage_map_actions = vec![
            "contains_key",
            "get",
            "try_get",
            "swap",
            "insert",
            "remove",
            "mutate",
            "try_mutate",
            "mutate_exists",
            "try_mutate_exists",
            "take",
            "append",
            "decode_len",
            "migrate_key",
            "remove_all",
            "iter_values",
            "translate_values",
            "try_append",
            "iter",
            "iter_from",
            "iter_keys",
            "iter_keys_from",
            "drain",
            "translate",
        ];
        let storage_map_actions_full = storage_map_actions
            .iter()
            .map(|e| "frame_support::StorageMap::".to_owned() + e)
            .collect::<Vec<_>>();
        storage_map_actions_full
    }

    pub fn from(action: &str) -> StorageMapActions {
        match action {
            "contains_key" => StorageMapActions::ContainsKey,
            "get" => StorageMapActions::Get,
            "try_get" => StorageMapActions::TryGet,
            "swap" => StorageMapActions::Swap,
            "insert" => StorageMapActions::Insert,
            "remove" => StorageMapActions::Remove,
            "mutate" => StorageMapActions::Mutate,
            "try_mutate" => StorageMapActions::TryMutate,
            "mutate_exists" => StorageMapActions::MutateExists,
            "try_mutate_exists" => StorageMapActions::TryMutateExists,
            "take" => StorageMapActions::Take,
            "append" => StorageMapActions::Append,
            "decode_len" => StorageMapActions::DecodeLen,
            "migrate_key" => StorageMapActions::MigrateKey,
            "remove_all" => StorageMapActions::RemoveAll,
            "iter_values" => StorageMapActions::IterValues,
            "translate_values" => StorageMapActions::TranslateValues,
            "try_append" => StorageMapActions::TryAppend,
            "iter" => StorageMapActions::Iter,
            "iter_from" => StorageMapActions::IterFrom,
            "iter_keys" => StorageMapActions::IterKeys,
            "iter_keys_from" => StorageMapActions::IterKeysFrom,
            "drain" => StorageMapActions::Drain,
            "translate" => StorageMapActions::Translate,
            _ => panic!("Invalid StorageMap action"),
        }
    }

    pub fn get_weights(&self) -> Weights {
        let default = Weights::default();
        match self {
            StorageMapActions::ContainsKey => default,
            StorageMapActions::Get => Weights::new(1, 0),
            StorageMapActions::TryGet => default,
            StorageMapActions::Swap => default,
            StorageMapActions::Insert => default,
            StorageMapActions::Remove => default,
            StorageMapActions::Mutate => default,
            StorageMapActions::TryMutate => default,
            StorageMapActions::MutateExists => default,
            StorageMapActions::TryMutateExists => default,
            StorageMapActions::Take => default,
            StorageMapActions::Append => default,
            StorageMapActions::DecodeLen => default,
            StorageMapActions::MigrateKey => default,
            StorageMapActions::RemoveAll => default,
            StorageMapActions::IterValues => default,
            StorageMapActions::TranslateValues => default,
            StorageMapActions::TryAppend => default,
            StorageMapActions::Iter => default,
            StorageMapActions::IterFrom => default,
            StorageMapActions::IterKeys => default,
            StorageMapActions::IterKeysFrom => default,
            StorageMapActions::Drain => default,
            StorageMapActions::Translate => default,
        }
    }
}

pub fn apply_r_w(tcx: TyCtxt, def_id: DefId, context: &mut Context) {
    let fn_full_name = get_fn_name_with_path(tcx, def_id);
    let fn_short_name = get_fn_name(tcx, def_id);

    let mut weights = Weights::default();

    if StorageValueActions::is_storage_value_action(&fn_full_name) {
        weights = StorageValueActions::from(&fn_short_name).get_weights();
    } else if StorageMapActions::is_storage_map_action(&fn_full_name) {
        weights = StorageMapActions::from(&fn_short_name).get_weights();
    }

    context.weights += weights
}
