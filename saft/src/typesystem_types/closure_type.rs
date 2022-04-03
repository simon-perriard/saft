use rustc_middle::ty::TyCtxt;

use crate::analysis_utils::def_id_printer::get_def_id_name_with_path;

pub fn get_closures(tcx: &TyCtxt) {
    for body_owner_local_def_id in tcx.hir().body_owners() {
        let body_owner_hir_id = tcx.hir().local_def_id_to_hir_id(body_owner_local_def_id);

        /*if get_def_id_name_with_path(*tcx, body_owner_local_def_id.to_def_id()).contains(&String::from("add_registrar")) {
            println!("{}, {:?}", get_def_id_name_with_path(*tcx, body_owner_local_def_id.to_def_id()), body_owner_local_def_id);
            let body_id = tcx.hir().body_owned_by(body_owner_hir_id);
            println!("{:?}", tcx.hir().body(body_id));
            println!("");
            println!("");
        }*/
    }
}
