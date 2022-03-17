use rustc_hir::def_id::LocalDefId;
use rustc_middle::ty::TyCtxt;

use crate::analysis_utils::{get_def_id_name, get_def_id_name_with_path};
#[derive(Clone, Debug)]
pub struct Ident {
    pub name_short: String,
    pub name_full: String,
}

#[derive(Clone, Debug)]
pub struct RootIdent {
    pub def_id: LocalDefId,
    pub name_short: String,
    pub name_full: String,
}

#[derive(Clone, Debug)]
pub struct FrameStorageType {
    pub alias_ident: Ident,
    pub kind: StorageKind,
}

#[derive(Clone, Debug)]
pub enum ValueType {
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,
    Option(Box<ValueType>),
    BoundedVec(Box<ValueType>),
    Get(Box<ValueType>),
    Symbol { symbol: String },
}

#[derive(Clone, Debug)]
pub enum StorageKind {
    StorageValue {
        ident: RootIdent,
        value: crate::storage_typesystem::ValueType,
    },
    StorageMap {
        ident: RootIdent,
        value: crate::storage_typesystem::ValueType,
        max_value: Option<ValueType>,
    },
    StorageDoubleMap {
        ident: RootIdent,
        value: crate::storage_typesystem::ValueType,
        max_value: Option<ValueType>,
    },
    StorageNMap {
        ident: RootIdent,
        value: crate::storage_typesystem::ValueType,
        max_value: Option<ValueType>,
    },
    Dummy,
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

    /*pub fn visit_display(&self, tcx: &TyCtxt) {
        print!("{:?}", self.name);
        if self.children.len() > 0 {
            print!("<");

            let mut count = self.children.len() - 1;
            for child in self.children.iter() {
                child.visit_display(tcx);
                if count > 0 {
                    print!(", ");
                }
                count -= 1;
            }

            print!(">");
        }
    }*/
}
