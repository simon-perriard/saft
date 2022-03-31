use crate::analysis_utils::def_id_printer::*;
use crate::typesystem_types::array_type::Array;
use crate::typesystem_types::boundedvec_type::BoundedVec;
use crate::typesystem_types::function_type::Function;
use crate::typesystem_types::primitive_types::{Float, Int, Primitive, PrimitiveSize, Uint};
use crate::typesystem_types::slice_type::Slice;
use crate::typesystem_types::struct_type::Struct;
use crate::typesystem_types::tuple_type::Tuple;
use crate::typesystem_types::typesystem_config_constant_types::PalletConfigConstantType;
use crate::typesystem_types::typesystem_declared_types::PalletDeclaredType;
use crate::typesystem_types::typesystem_storage::FrameStorageType;
use core::fmt;
use rpds::HashTrieMap;
use rustc_ast::ast::*;
use rustc_hir::def::{DefKind, Res};
use rustc_hir::def_id::DefId;
use rustc_hir::PrimTy;
use rustc_middle::mir::interpret;
use rustc_middle::ty::TyCtxt;

#[derive(Clone, Debug)]
/// Textual identity of a type, aka its name
pub struct Identifier {
    pub def_id: DefId,
}

impl Identifier {
    pub fn get_name(&self, tcx: &TyCtxt) -> String {
        get_def_id_name(*tcx, self.def_id)
    }

    pub fn get_name_full(&self, tcx: &TyCtxt) -> String {
        get_def_id_name_with_path(*tcx, self.def_id)
    }
}

#[derive(Debug, Clone)]
/// The atomic way to represent a size for a type
/// It can either be a concrete or a symbolic size
pub enum SizeType {
    Concrete(u128),
    Symbolic(String),
    Composite(Box<CompositeSize>),
}

// instead of size type
// have a small language to express symbolic bounds

impl Default for SizeType {
    fn default() -> Self {
        SizeType::Concrete(0)
    }
}

impl fmt::Display for SizeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SizeType::Concrete(c) => write!(f, "{}", c),
            SizeType::Symbolic(s) => write!(f, "{}", s),
            SizeType::Composite(cs) => write!(f, "{}", cs),
        }
    }
}

#[derive(Clone, Debug)]
/// A composite size, it can contain different elements to describe the size of a type
pub struct CompositeSize {
    /// Multiplication factor associated to the size,
    /// this can be used for an array that has a max size.
    /// Example:
    ///     BoundedVec<u8, MAX::Type>
    ///     will be represented by CompSize as
    ///     mul_factor: MAX::Type'size and concrete 1 (byte)
    pub mul_factor: SizeType,

    pub sizes: Vec<SizeType>,
}

impl Default for CompositeSize {
    fn default() -> Self {
        CompositeSize {
            mul_factor: SizeType::Concrete(1),
            sizes: Vec::new(),
        }
    }
}

impl CompositeSize {
    pub fn reduce_concrete(&mut self) {
        let mut acc = SizeType::default();

        self.sizes = self
            .sizes
            .drain_filter(|size| {
                match *size {
                    SizeType::Concrete(c) => {
                        // update accumulator and filter out
                        if let SizeType::Concrete(accum_size) = acc {
                            acc = SizeType::Concrete(accum_size + c);
                        } else {
                            unreachable!();
                        }

                        false
                    }
                    _ => true,
                }
            })
            .collect();

        // Add back the accumulated concrete
        self.sizes.push(acc);
    }
}

impl fmt::Display for CompositeSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fmt = String::new();

        let mut count = self.sizes.len() - 1;

        for size in self.sizes.iter() {
            match size {
                SizeType::Concrete(c) => fmt.push_str(&c.to_string()),
                SizeType::Symbolic(s) => fmt.push_str(s),
                SizeType::Composite(cs) => fmt.push_str(&format!("{}", cs)),
            }

            if count > 0 {
                fmt.push_str(" + ")
            }
            count -= 1;
        }

        match self.mul_factor {
            SizeType::Concrete(0) => {
                write!(f, "")
            }
            SizeType::Concrete(1) => {
                write!(f, "{}", fmt)
            }
            _ => {
                write!(f, "{} * ({})", self.mul_factor, fmt)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum TraitOrType {
    Trait(Trait),
    Type(Type),
}

impl TraitOrType {
    pub fn expect_type(self) -> Type {
        match self {
            TraitOrType::Trait(_) => unreachable!(),
            TraitOrType::Type(t) => t,
        }
    }

    pub fn expect_trait(self) -> Trait {
        match self {
            TraitOrType::Trait(t) => t,
            TraitOrType::Type(_) => unreachable!(),
        }
    }
}

impl TraitOrType {
    pub fn collect_size(&self) -> SizeType {
        match self {
            TraitOrType::Trait(t) => t.collect_size(),
            TraitOrType::Type(t) => t.collect_size(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Trait {
    Get(Type),
    Symbol { full_name: String, size: SizeType },
}

impl Trait {
    pub fn collect_size(&self) -> SizeType {
        match self {
            Trait::Get(t) => t.collect_size(),
            Trait::Symbol { size, .. } => size.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    Primitive(Primitive),
    Option(Box<Type>),
    Tuple(Tuple),
    BoundedVec(BoundedVec),
    Array(Array),
    Slice(Slice),
    Function(Function),
    Struct(Struct),
    Symbol { full_name: String, size: SizeType },
    Unit,
}

impl Type {
    pub fn collect_size(&self) -> SizeType {
        match self {
            Type::Primitive(ty) => ty.collect_size(),
            Type::Option(ty) => ty.collect_size(),
            Type::Tuple(ty) => ty.size.clone(),
            Type::BoundedVec(ty) => ty.size.clone(),
            Type::Array(ty) => ty.size.clone(),
            Type::Slice(ty) => ty.size.clone(),
            Type::Function(ty) => ty.size.clone(),
            Type::Struct(ty) => ty.size.clone(),
            Type::Symbol { size, .. } => size.clone(),
            Type::Unit => SizeType::Concrete(0),
        }
    }
}

#[derive(Debug)]
pub enum TypeVariant {
    FrameStorageType(FrameStorageType),
    PalletDeclaredType(PalletDeclaredType),
    PalletConfigConstantType(PalletConfigConstantType),
}

pub trait Alias {
    fn get_size(&self) -> SizeType;

    fn get_name(&self, tcx: &TyCtxt) -> String;

    fn get_name_full(&self, tcx: &TyCtxt) -> String;
}

impl Alias for TypeVariant {
    fn get_size(&self) -> SizeType {
        match self {
            TypeVariant::FrameStorageType(ty) => ty.get_size(),
            TypeVariant::PalletDeclaredType(ty) => ty.get_size(),
            TypeVariant::PalletConfigConstantType(ty) => ty.get_size(),
        }
    }

    fn get_name(&self, tcx: &TyCtxt) -> String {
        match self {
            TypeVariant::FrameStorageType(ty) => ty.get_name(tcx),
            TypeVariant::PalletDeclaredType(ty) => ty.get_name(tcx),
            TypeVariant::PalletConfigConstantType(ty) => ty.get_name(tcx),
        }
    }

    fn get_name_full(&self, tcx: &TyCtxt) -> String {
        match self {
            TypeVariant::FrameStorageType(ty) => ty.get_name_full(tcx),
            TypeVariant::PalletDeclaredType(ty) => ty.get_name_full(tcx),
            TypeVariant::PalletConfigConstantType(ty) => ty.get_name_full(tcx),
        }
    }
}

/// Struct containing information about the types
/// declared in the pallet
pub struct TySys {
    pub tsm: HashTrieMap<String, TypeVariant>,
}

impl TySys {
    pub fn new() -> TySys {
        TySys {
            tsm: HashTrieMap::new(),
        }
    }

    pub fn add_type(&mut self, ty: TypeVariant, tcx: &TyCtxt) {
        self.tsm.insert_mut(ty.get_name_full(tcx), ty);
    }
}

impl Default for TySys {
    fn default() -> Self {
        Self::new()
    }
}

pub fn explore(tcx: &TyCtxt, ty: &rustc_hir::Ty, ts: &TySys) -> TraitOrType {
    match &ty.kind {
        rustc_hir::TyKind::Path(qpath) => {
            match qpath {
                rustc_hir::QPath::Resolved(_, path) => get_value_type(tcx, path, ts),
                rustc_hir::QPath::TypeRelative(ty, segment) => {
                    // Get super type as well
                    let super_type = if let rustc_hir::Ty{ kind: rustc_hir::TyKind::Path(qpath), .. } = ty
                    && let rustc_hir::QPath::Resolved(_, path) = qpath
                    && let rustc_hir::Path { segments, .. } = path {
                        segments[0].ident.as_str()
                    } else {
                        unreachable!();
                    };

                    // Check whether the symbol has been resolved before
                    let key = super_type.to_owned() + "::" + segment.ident.as_str();
                    if ts.tsm.contains_key(&key) {
                        match ts.tsm.get(&key).unwrap() {
                            TypeVariant::PalletDeclaredType(t) => {
                                TraitOrType::Type(t.value.clone())
                            }
                            TypeVariant::PalletConfigConstantType(t) => {
                                TraitOrType::Trait(t.trait_bound.clone())
                            }
                            TypeVariant::FrameStorageType(_) => unreachable!(),
                        }
                    } else {
                        // Simply return it as a symbol, TypeRelative paths may be resolved in later work
                        // Resolve type to defId
                        TraitOrType::Type(Type::Symbol {
                            full_name: super_type.to_owned() + "::" + segment.ident.as_str(),
                            size: SizeType::Symbolic(
                                super_type.to_owned() + "::" + segment.ident.as_str(),
                            ),
                        })
                    }
                }
                _ => unreachable!(),
            }
        }
        rustc_hir::TyKind::Tup(tys) => {
            // Block for Tuple
            let mut members = Vec::new();

            for ty in tys.iter() {
                members.push(explore(tcx, ty, ts).expect_type());
            }

            TraitOrType::Type(Type::Tuple(Tuple::new(members)))
        }
        rustc_hir::TyKind::Array(ty, len) => {
            // Block for Array with constant size
            if let rustc_hir::ArrayLen::Body(annon_const) = len
            && let rustc_hir::AnonConst { hir_id, .. } = annon_const
            {
                // Try to evaluate the constant expression
                if let Ok(const_eval) = tcx.const_eval_poly(tcx.hir().local_def_id(*hir_id).to_def_id()) {
                    match const_eval {
                        interpret::ConstValue::Scalar(scalar) => {
                            if let interpret::Scalar::Int(scalar_int) = scalar
                            && let Ok(value) = scalar_int.to_bits(scalar_int.size()) {
                                return TraitOrType::Type(Type::Array(
                                    Array::new(
                                    explore(tcx, ty, ts).expect_type(),
                                    SizeType::Concrete(value)
                                )));
                            } else {
                                unreachable!();
                            }
                        },
                        _ => unimplemented!(),
                    }
                }
            }
            unreachable!()
        }
        rustc_hir::TyKind::Rptr(_, mut_ty) => explore(tcx, mut_ty.ty, ts),
        rustc_hir::TyKind::Slice(ty) => {
            TraitOrType::Type(Type::Slice(Slice::new(explore(tcx, ty, ts).expect_type())))
        }
        rustc_hir::TyKind::BareFn(bare_fn_ty) => {
            let rustc_hir::BareFnTy { decl, .. } = bare_fn_ty;

            let mut inputs = Vec::new();

            for input_ty in decl.inputs.iter() {
                inputs.push(explore(tcx, input_ty, ts).expect_type());
            }

            let output = match decl.output {
                rustc_hir::FnRetTy::DefaultReturn(_) => Type::Unit,
                rustc_hir::FnRetTy::Return(ty) => explore(tcx, ty, ts).expect_type(),
            };

            TraitOrType::Type(Type::Function(Function::new(inputs, output)))
        }
        _ => {
            //println!("{:?}", ty);
            unreachable!()
        }
    }
}

/// Find the ValueType enum member that corresponds to the given path
pub fn get_value_type(tcx: &TyCtxt, path: &rustc_hir::Path, ts: &TySys) -> TraitOrType {
    let rustc_hir::Path { segments, res, .. } = path;

    match res {
        Res::Def(def_kind, def_id) => {
            match get_def_id_name_with_path(*tcx, *def_id).as_str() {
                // BoundedVec is a standard type in FRAME:
                // https://docs.substrate.io/rustdocs/latest/frame_support/storage/bounded_vec/struct.BoundedVec.html
                "frame_support::BoundedVec" => {
                    let generics = segments[0].args.unwrap().args;
                    if let rustc_hir::GenericArg::Type(ty_0) = &generics[0] {
                        let value = explore(tcx, ty_0, ts).expect_type();

                        return TraitOrType::Type(Type::BoundedVec(BoundedVec::new(value)));
                    }

                    unreachable!()
                }
                "std::option::Option" => {
                    let generic = &segments[0].args.unwrap().args[0];

                    if let rustc_hir::GenericArg::Type(ty) = generic {
                        return TraitOrType::Type(Type::Option(Box::new(
                            explore(tcx, ty, ts).expect_type(),
                        )));
                    }

                    unreachable!()
                }
                "frame_support::traits::Get" => {
                    let generic = &segments[0].args.unwrap().args[0];

                    if let rustc_hir::GenericArg::Type(ty) = generic {
                        return TraitOrType::Trait(Trait::Get(explore(tcx, ty, ts).expect_type()));
                    }

                    unreachable!()
                }
                _ => {
                    //println!("{}", get_def_id_name_with_path(*tcx, *def_id).as_str());
                    // Treat every unkown as symbol, later work will maybe resolve those
                    let key = get_def_id_name_with_path(*tcx, *def_id);
                    if ts.tsm.contains_key(&key) {
                        match ts.tsm.get(&key).unwrap() {
                            TypeVariant::PalletDeclaredType(t) => {
                                TraitOrType::Type(t.value.clone())
                            }
                            TypeVariant::PalletConfigConstantType(t) => {
                                TraitOrType::Trait(t.trait_bound.clone())
                            }
                            TypeVariant::FrameStorageType(_) => unreachable!(),
                        }
                    } else if let DefKind::Trait = def_kind {
                        TraitOrType::Trait(Trait::Symbol {
                            full_name: get_def_id_name_with_path(*tcx, *def_id),
                            size: SizeType::Symbolic(get_def_id_name_with_path(*tcx, *def_id)),
                        })
                    } else {
                        TraitOrType::Type(Type::Symbol {
                            full_name: get_def_id_name_with_path(*tcx, *def_id),
                            size: SizeType::Symbolic(get_def_id_name_with_path(*tcx, *def_id)),
                        })
                    }
                }
            }
        }
        // Primitive types
        // https://doc.rust-lang.org/reference/type-layout.html#primitive-data-layout
        Res::PrimTy(prim_ty) => match prim_ty {
            PrimTy::Int(int_ty) => match int_ty {
                IntTy::Isize => TraitOrType::Type(Type::Primitive(Primitive::Int(Int::Isize(
                    PrimitiveSize::new(),
                )))),
                IntTy::I8 => TraitOrType::Type(Type::Primitive(Primitive::Int(Int::I8(
                    PrimitiveSize::new(),
                )))),
                IntTy::I16 => TraitOrType::Type(Type::Primitive(Primitive::Int(Int::I16(
                    PrimitiveSize::new(),
                )))),
                IntTy::I32 => TraitOrType::Type(Type::Primitive(Primitive::Int(Int::I32(
                    PrimitiveSize::new(),
                )))),
                IntTy::I64 => TraitOrType::Type(Type::Primitive(Primitive::Int(Int::I64(
                    PrimitiveSize::new(),
                )))),
                IntTy::I128 => TraitOrType::Type(Type::Primitive(Primitive::Int(Int::I128(
                    PrimitiveSize::new(),
                )))),
            },
            PrimTy::Uint(uint_ty) => match uint_ty {
                UintTy::Usize => TraitOrType::Type(Type::Primitive(Primitive::Uint(Uint::Usize(
                    PrimitiveSize::new(),
                )))),
                UintTy::U8 => TraitOrType::Type(Type::Primitive(Primitive::Uint(Uint::U8(
                    PrimitiveSize::new(),
                )))),
                UintTy::U16 => TraitOrType::Type(Type::Primitive(Primitive::Uint(Uint::U16(
                    PrimitiveSize::new(),
                )))),
                UintTy::U32 => TraitOrType::Type(Type::Primitive(Primitive::Uint(Uint::U32(
                    PrimitiveSize::new(),
                )))),
                UintTy::U64 => TraitOrType::Type(Type::Primitive(Primitive::Uint(Uint::U64(
                    PrimitiveSize::new(),
                )))),
                UintTy::U128 => TraitOrType::Type(Type::Primitive(Primitive::Uint(Uint::U128(
                    PrimitiveSize::new(),
                )))),
            },
            PrimTy::Float(float_ty) => match float_ty {
                FloatTy::F32 => TraitOrType::Type(Type::Primitive(Primitive::Float(Float::F32(
                    PrimitiveSize::new(),
                )))),
                FloatTy::F64 => TraitOrType::Type(Type::Primitive(Primitive::Float(Float::F64(
                    PrimitiveSize::new(),
                )))),
            },
            PrimTy::Str => TraitOrType::Type(Type::Primitive(Primitive::Str(PrimitiveSize::new()))),
            PrimTy::Bool => {
                TraitOrType::Type(Type::Primitive(Primitive::Bool(PrimitiveSize::new())))
            }
            PrimTy::Char => {
                TraitOrType::Type(Type::Primitive(Primitive::Char(PrimitiveSize::new())))
            }
        },
        _ => todo!(),
    }
}
