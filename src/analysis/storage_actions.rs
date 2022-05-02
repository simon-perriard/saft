// https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageValue.html
// https://docs.substrate.io/rustdocs/latest/frame_support/storage/types/struct.StorageMap.html

use super::pallet::Field;

#[derive(Debug)]
pub(crate) enum AccessType {
    Read,
    Write,
    Both,
}

pub(crate) trait HasAccessType {
    fn get_access_type(&self, action: &str) -> Option<AccessType>;
}

impl HasAccessType for Field {
    fn get_access_type(&self, action: &str) -> Option<AccessType> {
        match &self.kind {
            super::pallet::StorageKind::StorageValue { .. } => {
                StorageValueActions::get_access_type(action)
            }
            super::pallet::StorageKind::StorageMap { .. } => {
                StorageMapActions::get_access_type(action)
            }
            super::pallet::StorageKind::StorageDoubleMap { .. } => {
                StorageDoubleMapActions::get_access_type(action)
            }
            super::pallet::StorageKind::StorageNMap { .. } => todo!(),
            super::pallet::StorageKind::CountedStorageMap { .. } => todo!(),
        }
    }
}

pub(crate) enum StorageValueActions {
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
    fn is_storage_value_action(action: &str) -> bool {
        StorageValueActions::storage_value_actions().contains(&action.to_owned())
    }

    fn storage_value_actions() -> Vec<String> {
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

        storage_value_actions
            .iter()
            .map(|action| String::from(*action))
            .collect()
    }

    fn from(action: &str) -> StorageValueActions {
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

    pub fn get_access_type(action: &str) -> Option<AccessType> {
        let action_short = action.clone().split("::").last().unwrap();

        if !Self::is_storage_value_action(action_short) {
            None
        } else {
            match Self::from(action_short) {
                StorageValueActions::Exists
                | StorageValueActions::Get
                | StorageValueActions::TryGet
                | StorageValueActions::DecodeLen => Some(AccessType::Read),
                StorageValueActions::Put | StorageValueActions::Set | StorageValueActions::Kill => {
                    Some(AccessType::Write)
                }
                StorageValueActions::Mutate
                | StorageValueActions::TryMutate
                | StorageValueActions::Take
                | StorageValueActions::Append
                | StorageValueActions::TryAppend
                | StorageValueActions::Translate => Some(AccessType::Both),
            }
        }
    }
}

pub(crate) enum StorageMapActions {
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

        storage_map_actions
            .iter()
            .map(|action| String::from(*action))
            .collect()
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

    pub fn get_access_type(action: &str) -> Option<AccessType> {
        let action_short = action.clone().split("::").last().unwrap();

        if !Self::is_storage_map_action(action_short) {
            None
        } else {
            match Self::from(action_short) {
                StorageMapActions::ContainsKey
                | StorageMapActions::Get
                | StorageMapActions::TryGet
                | StorageMapActions::DecodeLen => Some(AccessType::Read),

                StorageMapActions::Swap => Some(AccessType::Both),
                StorageMapActions::Insert => Some(AccessType::Write),
                StorageMapActions::Remove => Some(AccessType::Write),
                StorageMapActions::Mutate => Some(AccessType::Both),
                StorageMapActions::TryMutate => Some(AccessType::Both),
                StorageMapActions::MutateExists => Some(AccessType::Both),
                StorageMapActions::TryMutateExists => Some(AccessType::Both),
                StorageMapActions::Take => Some(AccessType::Both),
                StorageMapActions::Append => Some(AccessType::Both),
                StorageMapActions::MigrateKey => Some(AccessType::Both),
                StorageMapActions::RemoveAll => Some(AccessType::Write),
                StorageMapActions::IterValues => Some(AccessType::Read),
                StorageMapActions::TranslateValues => Some(AccessType::Both),
                StorageMapActions::TryAppend => Some(AccessType::Both),
                StorageMapActions::Iter => Some(AccessType::Read),
                StorageMapActions::IterFrom => Some(AccessType::Read),
                StorageMapActions::IterKeys => Some(AccessType::Read),
                StorageMapActions::IterKeysFrom => Some(AccessType::Read),
                StorageMapActions::Drain => Some(AccessType::Both),
                StorageMapActions::Translate => Some(AccessType::Both),
            }
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
    pub fn is_storage_map_action(action: &str) -> bool {
        StorageDoubleMapActions::storage_double_map_actions().contains(&action.to_owned())
    }

    pub fn storage_double_map_actions() -> Vec<String> {
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

    pub fn from(action: &str) -> StorageDoubleMapActions {
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
            _ => panic!("Invalid StorageMap action"),
        }
    }

    pub fn get_access_type(action: &str) -> Option<AccessType> {
        let action_short = action.clone().split("::").last().unwrap();

        if !Self::is_storage_map_action(action_short) {
            None
        } else {
            match Self::from(action_short) {
                StorageDoubleMapActions::Append => Some(AccessType::Both),
                StorageDoubleMapActions::ContainsKey => Some(AccessType::Read),
                StorageDoubleMapActions::DecodeLen => Some(AccessType::Read),
                StorageDoubleMapActions::Drain => Some(AccessType::Both),
                StorageDoubleMapActions::DrainPrefix => Some(AccessType::Both),
                StorageDoubleMapActions::Get => Some(AccessType::Read),
                StorageDoubleMapActions::Insert => Some(AccessType::Write),
                StorageDoubleMapActions::Iter => Some(AccessType::Read),
                StorageDoubleMapActions::IterFrom => Some(AccessType::Read),
                StorageDoubleMapActions::IterKeyPrefix => Some(AccessType::Read),
                StorageDoubleMapActions::IterKeyPrefixFrom => Some(AccessType::Read),
                StorageDoubleMapActions::IterKeys => Some(AccessType::Read),
                StorageDoubleMapActions::IterKeysFrom => Some(AccessType::Read),
                StorageDoubleMapActions::IterPrefix => Some(AccessType::Read),
                StorageDoubleMapActions::IterPrefixFrom => Some(AccessType::Read),
                StorageDoubleMapActions::IterPrefixValues => Some(AccessType::Read),
                StorageDoubleMapActions::IterValues => Some(AccessType::Read),
                StorageDoubleMapActions::MigrateKeys => Some(AccessType::Both),
                StorageDoubleMapActions::Mutate => Some(AccessType::Both),
                StorageDoubleMapActions::MutateExists => Some(AccessType::Both),
                StorageDoubleMapActions::Remove => Some(AccessType::Write),
                StorageDoubleMapActions::RemoveAll => Some(AccessType::Write),
                StorageDoubleMapActions::RemovePrefix => Some(AccessType::Write),
                StorageDoubleMapActions::Swap => Some(AccessType::Both),
                StorageDoubleMapActions::Take => Some(AccessType::Both),
                StorageDoubleMapActions::Translate => Some(AccessType::Both),
                StorageDoubleMapActions::TranslateValues => Some(AccessType::Both),
                StorageDoubleMapActions::TryAppend => Some(AccessType::Both),
                StorageDoubleMapActions::TryGet => Some(AccessType::Read),
                StorageDoubleMapActions::TryMutate => Some(AccessType::Both),
                StorageDoubleMapActions::TryMutateExists => Some(AccessType::Both),
            }
        }
    }
}
