use rustc_middle::mir::Mutability;
use rustc_middle::ty;
use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::DefId;

use super::size_language::Size;

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
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum Adt {
    Unknown(DefId),
    Vec(Box<Type>),
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
            _ => unimplemented!(),
        }
    }

    pub(crate) fn get_size(&self) -> Size {
        match self {
            Type::Bool => todo!(),
            Type::Char => todo!(),
            Type::Int(_) => todo!(),
            Type::Uint(_) => todo!(),
            Type::Float(_) => todo!(),
            Type::Adt(_) => todo!(),
            Type::Str => todo!(),
            Type::Array(_, _) => todo!(),
            Type::Slice(_) => todo!(),
            Type::Ref(_, _) => todo!(),
            Type::FnPtr(_, _) => todo!(),
            Type::Tuple(_) => todo!(),
            Type::Projection(_) => todo!(),
        }
    }
}
