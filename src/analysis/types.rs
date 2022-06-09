use rustc_middle::mir::Mutability;
use rustc_middle::ty;
use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::DefId;
use std::mem::size_of;

use super::cost_language::{Cost, HasSize, Symbolic};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum Type {
    Bool,
    Char,
    Int(ty::IntTy),
    Uint(ty::UintTy),
    Float(ty::FloatTy),
    Adt(Adt),
    Str,
    Array(Box<Type>, u64),
    Slice(Box<Type>),
    Ref(Box<Type>, Mutability),
    FnPtr(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Projection(DefId),
    Unsupported,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum Adt {
    Unknown(DefId),
    Option(Box<Type>),
    BoundedVec(Box<Type>, Box<Type>),
}

impl Type {
    pub(crate) fn from_mir_ty<'tcx>(tcx: TyCtxt<'tcx>, ty: ty::Ty<'tcx>) -> Self {
        use rustc_middle::ty::TyKind;
        match *ty.kind() {
            TyKind::Bool => Type::Bool,
            TyKind::Char => Type::Char,
            TyKind::Int(t) => Type::Int(t),
            TyKind::Uint(t) => Type::Uint(t),
            TyKind::Float(t) => Type::Float(t),
            TyKind::Adt(adt_def, substs) => match tcx.def_path_str(adt_def.did()).as_str() {
                "frame_support::BoundedVec" => {
                    let ty = Self::from_mir_ty(tcx, substs.type_at(0));
                    let max_size = Self::from_mir_ty(tcx, substs.type_at(1));
                    Type::Adt(Adt::BoundedVec(Box::new(ty), Box::new(max_size)))
                }
                "std::option::Option" => {
                    let ty = Self::from_mir_ty(tcx, substs.type_at(0));
                    Type::Adt(Adt::Option(Box::new(ty)))
                }
                _ => Type::Adt(Adt::Unknown(adt_def.did())),
            },
            TyKind::Str => Type::Str,
            TyKind::Array(t, size) => Type::Array(
                Box::new(Self::from_mir_ty(tcx, t)),
                size.val().try_to_machine_usize(tcx).unwrap(),
            ),
            TyKind::Slice(t) => Type::Slice(Box::new(Self::from_mir_ty(tcx, t))),
            TyKind::Ref(_, t, mutability) => {
                Type::Ref(Box::new(Self::from_mir_ty(tcx, t)), mutability)
            }
            TyKind::FnPtr(poly_fn_sig) => {
                let fn_sig = poly_fn_sig
                    .no_bound_vars()
                    .expect("Polymorphic functions not supported.");
                Type::FnPtr(
                    fn_sig
                        .inputs()
                        .iter()
                        .map(|&ty| Self::from_mir_ty(tcx, ty))
                        .collect(),
                    Box::new(Self::from_mir_ty(tcx, fn_sig.output())),
                )
            }
            TyKind::Tuple(_) => Type::Tuple(
                ty.tuple_fields()
                    .iter()
                    .map(|ty| Self::from_mir_ty(tcx, ty))
                    .collect(),
            ),
            TyKind::Projection(p) => Type::Projection(p.item_def_id),
            _ => Type::Unsupported,
        }
    }
}

impl HasSize for Type {
    fn get_size(&self, tcx: TyCtxt) -> Cost {
        match self {
            Type::Bool => Cost::Concrete(size_of::<bool>().try_into().unwrap()),
            Type::Char => Cost::Concrete(size_of::<char>().try_into().unwrap()),
            Type::Int(int_ty) => match int_ty {
                ty::IntTy::Isize => Cost::Concrete(size_of::<isize>().try_into().unwrap()),
                ty::IntTy::I8 => Cost::Concrete(size_of::<i8>().try_into().unwrap()),
                ty::IntTy::I16 => Cost::Concrete(size_of::<i16>().try_into().unwrap()),
                ty::IntTy::I32 => Cost::Concrete(size_of::<i32>().try_into().unwrap()),
                ty::IntTy::I64 => Cost::Concrete(size_of::<i64>().try_into().unwrap()),
                ty::IntTy::I128 => Cost::Concrete(size_of::<i128>().try_into().unwrap()),
            },
            Type::Uint(uint_ty) => match uint_ty {
                ty::UintTy::Usize => Cost::Concrete(size_of::<usize>().try_into().unwrap()),
                ty::UintTy::U8 => Cost::Concrete(size_of::<u8>().try_into().unwrap()),
                ty::UintTy::U16 => Cost::Concrete(size_of::<u16>().try_into().unwrap()),
                ty::UintTy::U32 => Cost::Concrete(size_of::<u32>().try_into().unwrap()),
                ty::UintTy::U64 => Cost::Concrete(size_of::<u64>().try_into().unwrap()),
                ty::UintTy::U128 => Cost::Concrete(size_of::<u128>().try_into().unwrap()),
            },
            Type::Float(float_ty) => match float_ty {
                ty::FloatTy::F32 => Cost::Concrete(size_of::<f32>().try_into().unwrap()),
                ty::FloatTy::F64 => Cost::Concrete(size_of::<f64>().try_into().unwrap()),
            },
            Type::Adt(adt) => match adt {
                Adt::Unknown(def_id) => {
                    let ty = tcx.type_of(def_id);
                    match tcx.layout_of(tcx.param_env(def_id).and(ty)) {
                        Ok(ty_and_layout) => Cost::Concrete(ty_and_layout.layout.size().bytes()),
                        Err(_) => {
                            let path = tcx.def_path_str(*def_id);
                            // extract the name of the type for readability
                            let path = path.split("::").last().unwrap().to_string();
                            Cost::Symbolic(Symbolic::SizeOf(path))
                        }
                    }
                }
                Adt::Option(ty) => ty.get_size(tcx),
                Adt::BoundedVec(ty, max_size) => {
                    let max_size = match **max_size {
                        // Type defined in the pallet
                        Type::Adt(Adt::Unknown(def_id)) |
                        // Type defined in the runtime
                        Type::Projection(def_id) => {
                            let path = tcx.def_path_str(def_id);
                            // extract the name of the type for readability
                            let path = path.split("::").last().unwrap();
                            Symbolic::ValueOf(format!("{}::get()", path))
                        },
                        _ => unreachable!(),
                    };

                    max_size.symbolic_mul(ty.get_size(tcx))
                }
            },
            Type::Str => todo!(),
            Type::Array(ty, size) => Cost::Concrete(*size).concrete_mul(ty.get_size(tcx)),
            Type::Slice(_) => todo!(),
            Type::Ref(ty, _) => ty.get_size(tcx),
            Type::FnPtr(_, ret_ty) => ret_ty.get_size(tcx),
            Type::Tuple(tys) => tys
                .iter()
                .map(|ty| ty.get_size(tcx))
                .reduce(|acc, ty_size| acc + ty_size)
                .unwrap_or_default(),
            Type::Projection(def_id) => {
                let path = tcx.def_path_str(*def_id);
                // extract the name of the type for readability
                let path = path.split("::").last().unwrap().to_string();
                Cost::Symbolic(Symbolic::SizeOf(path))
            }
            Type::Unsupported => panic!(), /*Cost::default()*/
        }
    }
}
