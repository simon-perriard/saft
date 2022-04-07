#![feature(rustc_private)]
#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(drain_filter)]

extern crate rustc_ast;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_span;
extern crate rustc_typeck;

pub mod analysis_utils;
pub mod dispatchable_visitor;
pub mod mir_visitor;
pub mod pallet;
pub mod size_language;
pub mod storage_actions;
pub mod types;
pub mod weights;

pub const SAFT_OPT_SEPARATOR: &str = "__OPT_SEPARATOR__";

#[derive(Default,Clone)]
pub struct SaftOptions {
    //pub single_dispatchable: Option<String>,
}

/*pub fn parse_options(options: Option<String>) -> SaftOptions {

    if let Some(options) = options {
        let mut single_dispatchable = None;

        let options_iter = options.split(SAFT_OPT_SEPARATOR);

        for option in options_iter {
            let parsed_option: Vec<&str> = option.split('=').collect();

            if parsed_option.len() != 2 {
                panic!("Malformed option.");
            }

            let option_name = parsed_option[0];
            let option_param = parsed_option[1];

            match option_name {
                "--single_dispatchable" => {
                    single_dispatchable = Some(option_param.to_owned());
                }
                _ => panic!("Invalid option")
            }
        }

        SaftOptions {
            single_dispatchable,
        }
    } else {
        return SaftOptions::default();
    }
}*/

pub fn extract_juice<'tcx>(_compiler: &rustc_interface::interface::Compiler, tcx: rustc_middle::ty::TyCtxt<'tcx>) {

    // Extract pallet
    let pallet = pallet::Pallet::new(tcx);

    /*if let Some(single_function) = options.single_dispatchable {
        println!("The following dispatchables will be analyzed :");
        println!("{}", single_function);

        let mut target_dispatchable_def_id = None;

        for id in pallet.dispatchables.keys() {
            if analysis_utils::def_id_printer::get_def_id_name(tcx, *id) == single_function.trim() {
                target_dispatchable_def_id = Some(id);
                break;
            }
        }

        if let Some(target_dispatchable_def_id) = target_dispatchable_def_id {
            let mut dispatchable_visitor =
            dispatchable_visitor::DispatchableVisitor::new(tcx, &pallet, *target_dispatchable_def_id);
            println!("Analyzing {}...", dispatchable_visitor.get_fn_name());
            dispatchable_visitor.visit_body();
        } else {
            println!("Function {} not found.", single_function);
            std::process::exit(1);
        }
    } else {*/
        /*println!("The following dispatchables will be analyzed :");
        print_dispatchables_names(tcx, pallet.functions);*/

        for dispatchables_def_id in pallet.dispatchables.keys() {
            let mut dispatchable_visitor = dispatchable_visitor::DispatchableVisitor::new(tcx, &pallet, *dispatchables_def_id);
            println!("Analyzing {}...", dispatchable_visitor.get_fn_name());
            dispatchable_visitor.visit_body();
        }
    //}
}