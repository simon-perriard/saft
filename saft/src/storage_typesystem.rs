use rustc_hir::def_id::LocalDefId;
use rustc_middle::ty::TyCtxt;

use crate::{analysis_utils::def_id_printer::*, typesystem_common::CompSize};
#[derive(Clone, Debug)]
pub struct Ident {
    pub name_short: String,
    pub name_full: String,
}

#[derive(Clone, Debug)]
pub struct FrameStorageType {
    pub alias_ident: Ident,
    pub kind: StorageKind,
}

#[derive(Clone, Debug)]
pub enum StorageKind {
    StorageValue {
        ident: Ident,
        value: crate::typesystem_common::ValueType,
    },
    StorageMap {
        ident: Ident,
        value: crate::typesystem_common::ValueType,
        //max_values: Option<ValueType>,
    },
    StorageDoubleMap {
        ident: Ident,
        value: crate::typesystem_common::ValueType,
        //max_values: Option<ValueType>,
    },
    StorageNMap {
        ident: Ident,
        value: crate::typesystem_common::ValueType,
        //max_values: Option<ValueType>,
    },
    CountedStorageMap {
        ident: Ident,
        value: crate::typesystem_common::ValueType,
        //max_values: Option<ValueType>,
    },
}

impl FrameStorageType {
    pub fn new(tcx: &TyCtxt, local_def_id: LocalDefId, kind: StorageKind) -> Self {
        FrameStorageType {
            alias_ident: Ident {
                name_short: get_def_id_name(*tcx, local_def_id.to_def_id()),
                name_full: get_def_id_name_with_path(*tcx, local_def_id.to_def_id()),
            },
            kind,
        }
    }

    pub fn get_size(&self) -> CompSize {
        match &self.kind {
            StorageKind::StorageValue { value, .. } => value.get_size(),
            StorageKind::StorageMap { value, .. } => value.get_size(),
            StorageKind::StorageDoubleMap { value, .. } => value.get_size(),
            StorageKind::StorageNMap { value, .. } => value.get_size(),
            StorageKind::CountedStorageMap { value, .. } => value.get_size(),
        }
    }
}
