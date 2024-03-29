use rustc_middle::mir::Mutability;
use rustc_middle::ty;
use rustc_middle::ty::{ConstKind, TyCtxt};
use rustc_span::def_id::DefId;
use std::mem::size_of;

use super::cost_language::{Cost, CostParameter, HasSize};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum Type {
    Bool,
    Char,
    Int(ty::IntTy),
    Uint(ty::UintTy),
    Float(ty::FloatTy),
    Adt(Adt),
    Array(Box<Type>, Cost),
    Ref(Box<Type>, Mutability),
    RawPtr(Box<Type>, Mutability),
    Slice(Box<Type>),
    Opaque(Box<Type>),
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
            TyKind::Adt(adt_def, substs) => {
                // Expose type if it is boxed
                if ty.is_box() {
                    return Self::from_mir_ty(tcx, ty.boxed_ty());
                }

                match tcx.def_path_str(adt_def.did()).as_str() {
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
                }
            }
            TyKind::Array(t, size) => {
                let size = if let Some(size) = size.val().try_to_machine_usize(tcx) {
                    Cost::Scalar(size)
                } else if let ConstKind::Unevaluated(uneval) = size.val() {
                    Cost::Parameter(CostParameter::ValueOf(tcx.def_path_str(uneval.def.did)))
                } else {
                    panic!()
                };

                Type::Array(Box::new(Self::from_mir_ty(tcx, t)), size)
            }
            TyKind::Slice(t) => Type::Slice(Box::new(Self::from_mir_ty(tcx, t))),
            TyKind::Ref(_, t, mutability) => {
                Type::Ref(Box::new(Self::from_mir_ty(tcx, t)), mutability)
            }
            TyKind::RawPtr(type_and_mut) => Type::RawPtr(
                Box::new(Self::from_mir_ty(tcx, type_and_mut.ty)),
                type_and_mut.mutbl,
            ),
            TyKind::Tuple(_) => Type::Tuple(
                ty.tuple_fields()
                    .iter()
                    .map(|ty| Self::from_mir_ty(tcx, ty))
                    .collect(),
            ),
            TyKind::Opaque(t, _) => Type::Opaque(Box::new(Self::from_mir_ty(tcx, tcx.type_of(t)))),
            TyKind::Projection(p) => Type::Projection(p.item_def_id),
            _ => {
                println!("{:?}", ty.kind());
                Type::Unsupported
            }
        }
    }
}

impl HasSize for Type {
    fn get_size(&self, tcx: TyCtxt) -> Cost {
        match self {
            Type::Bool => Cost::Scalar(size_of::<bool>().try_into().unwrap()),
            Type::Char => Cost::Scalar(size_of::<char>().try_into().unwrap()),
            Type::Int(int_ty) => match int_ty {
                ty::IntTy::Isize => Cost::Scalar(size_of::<isize>().try_into().unwrap()),
                ty::IntTy::I8 => Cost::Scalar(size_of::<i8>().try_into().unwrap()),
                ty::IntTy::I16 => Cost::Scalar(size_of::<i16>().try_into().unwrap()),
                ty::IntTy::I32 => Cost::Scalar(size_of::<i32>().try_into().unwrap()),
                ty::IntTy::I64 => Cost::Scalar(size_of::<i64>().try_into().unwrap()),
                ty::IntTy::I128 => Cost::Scalar(size_of::<i128>().try_into().unwrap()),
            },
            Type::Uint(uint_ty) => match uint_ty {
                ty::UintTy::Usize => Cost::Scalar(size_of::<usize>().try_into().unwrap()),
                ty::UintTy::U8 => Cost::Scalar(size_of::<u8>().try_into().unwrap()),
                ty::UintTy::U16 => Cost::Scalar(size_of::<u16>().try_into().unwrap()),
                ty::UintTy::U32 => Cost::Scalar(size_of::<u32>().try_into().unwrap()),
                ty::UintTy::U64 => Cost::Scalar(size_of::<u64>().try_into().unwrap()),
                ty::UintTy::U128 => Cost::Scalar(size_of::<u128>().try_into().unwrap()),
            },
            Type::Float(float_ty) => match float_ty {
                ty::FloatTy::F32 => Cost::Scalar(size_of::<f32>().try_into().unwrap()),
                ty::FloatTy::F64 => Cost::Scalar(size_of::<f64>().try_into().unwrap()),
            },
            Type::Adt(adt) => match adt {
                Adt::Unknown(def_id) => {
                    let ty = tcx.type_of(def_id);
                    match tcx.layout_of(tcx.param_env(def_id).and(ty)) {
                        Ok(ty_and_layout) => Cost::Scalar(ty_and_layout.layout.size().bytes()),
                        Err(_) => {
                            let path = tcx.def_path_str(*def_id);

                            // extract the name of the type for readability
                            let path = path.split("::").last().unwrap().to_string();
                            Cost::Parameter(CostParameter::SizeOf(path))
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
                            CostParameter::ValueOf(format!("{}::get()", path))
                        },
                        _ => unreachable!(),
                    };
                    // We check that the size of 1 element is bounded
                    assert!(!ty.get_size(tcx).is_infinity());
                    max_size.symbolic_mul(ty.get_size(tcx))
                }
            },
            Type::Array(ty, size) => size.clone().mul(ty.get_size(tcx)),
            Type::Ref(ty, _) => ty.get_size(tcx),
            Type::RawPtr(ty, _) => ty.get_size(tcx),
            Type::Slice(_) => Cost::Infinity,
            Type::Tuple(tys) => tys
                .iter()
                .map(|ty| ty.get_size(tcx))
                .reduce(|acc, ty_size| acc + ty_size)
                .unwrap_or_default(),
            Type::Projection(def_id) => {
                let path = tcx.def_path_str(*def_id);
                // extract the name of the type for readability
                let path = path.split("::").last().unwrap().to_string();
                Cost::Parameter(CostParameter::SizeOf(path))
            }
            Type::Opaque(ty) => ty.get_size(tcx),
            Type::Unsupported => panic!(),
        }
    }
}
