use rustc_middle::ty::TyCtxt;

use crate::typesystem_common::*;
use crate::typesystem_types::struct_type::Struct;

use super::typesystem_storage::get_storage_variables_names;

pub fn get_innertypes(tcx: &TyCtxt) {
    // Start with outermost type declaration as they may be used
    // for other type def in the Config trait
    for body_owner in tcx.hir().body_owners() {
        let body_id = tcx.hir().body_owned_by(tcx.hir().local_def_id_to_hir_id(body_owner));
        let body = tcx.hir().body(body_id);

        if let rustc_hir::Body { value, .. } = body
        && let rustc_hir::Expr { kind, .. } = value
        && let rustc_hir::ExprKind::Closure(capture_by, _, _, _, _) = kind
        {
            println!("HAAAAAAAAAAAA");
        }
    }
}
