use crate::{
    analysis_utils::def_id_printer::*, typesystem_config_constant_types::PalletConfigConstantType,
    typesystem_declared_types::PalletDeclaredType, typesystem_storage::FrameStorageType,
};
use core::fmt;
use rpds::HashTrieMap;
use rustc_ast::ast::*;
use rustc_hir::def::Res;
use rustc_hir::PrimTy;
use rustc_middle::mir::interpret;
use rustc_middle::ty::TyCtxt;

use self::{
    array_type::ArrayTy,
    bounded_vec_type::BoundedVec,
    primitive_types::*,
    tuple_type::TupleTy,
};

#[derive(Debug, Clone)]
/// The atomic way to represent a size for a type
/// It can either be a concrete or a symbolic size
pub enum SizeType {
    Concrete(u128),
    Symbolic(String),
    Composite(Box<CompositeSize>),
}

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
                SizeType::Symbolic(s) => fmt.push_str(&s),
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
pub enum ValueTypeV2 {
    PrimTy(SaftPrimTy),
    Option(Box<ValueTypeV2>),
    Tuple(TupleTy),
    BoundedVec(BoundedVec),
    Array(ArrayTy),
    Slice,
    /*ConstGet(GetTrait),*/
    Fn,
    Symbol { symbol: String, size: SizeType },
    DefaultRet,
}

impl ValueTypeV2 {
    pub fn collect_size(&self) -> SizeType {
        match self {
            ValueTypeV2::PrimTy(ty) => ty.collect_size(),
            ValueTypeV2::Option(ty) => ty.collect_size(),
            ValueTypeV2::Tuple(ty) => ty.size.clone(),
            ValueTypeV2::BoundedVec(_) => todo!(),
            ValueTypeV2::Array(ty) => ty.size.clone(),
            ValueTypeV2::Slice => todo!(),
            ValueTypeV2::Fn => todo!(),
            ValueTypeV2::Symbol { size, .. } => size.clone(),
            ValueTypeV2::DefaultRet => SizeType::Concrete(0),
        }
    }
}

pub mod primitive_types {
    use super::*;
    use std::{marker::PhantomData, mem};

    const BYTE: usize = 8;

    #[derive(Clone, Debug)]
    pub enum SaftPrimTy {
        Int(SaftIntTy),
        Uint(SaftUintTy),
        Float(SaftFloatTy),
        Str(SaftPrimTySize<&'static str>),
        Bool(SaftPrimTySize<bool>),
        Char(SaftPrimTySize<char>),
    }

    impl SaftPrimTy {
        pub fn collect_size(&self) -> SizeType {
            match self {
                SaftPrimTy::Int(ty) => ty.collect_size(),
                SaftPrimTy::Uint(ty) => ty.collect_size(),
                SaftPrimTy::Float(ty) => ty.collect_size(),
                SaftPrimTy::Str(s) => s.size.clone(),
                SaftPrimTy::Bool(s) => s.size.clone(),
                SaftPrimTy::Char(s) => s.size.clone(),
            }
        }
    }

    #[derive(Clone, Debug)]
    pub enum SaftIntTy {
        Isize(SaftPrimTySize<isize>),
        I8(SaftPrimTySize<i8>),
        I16(SaftPrimTySize<i16>),
        I32(SaftPrimTySize<i32>),
        I64(SaftPrimTySize<i64>),
        I128(SaftPrimTySize<i128>),
    }

    impl SaftIntTy {
        pub fn collect_size(&self) -> SizeType {
            match self {
                SaftIntTy::Isize(s) => s.size.clone(),
                SaftIntTy::I8(s) => s.size.clone(),
                SaftIntTy::I16(s) => s.size.clone(),
                SaftIntTy::I32(s) => s.size.clone(),
                SaftIntTy::I64(s) => s.size.clone(),
                SaftIntTy::I128(s) => s.size.clone(),
            }
        }
    }

    #[derive(Clone, Debug)]
    pub enum SaftUintTy {
        Usize(SaftPrimTySize<usize>),
        U8(SaftPrimTySize<u8>),
        U16(SaftPrimTySize<u16>),
        U32(SaftPrimTySize<u32>),
        U64(SaftPrimTySize<u64>),
        U128(SaftPrimTySize<u128>),
    }

    impl SaftUintTy {
        pub fn collect_size(&self) -> SizeType {
            match self {
                SaftUintTy::Usize(s) => s.size.clone(),
                SaftUintTy::U8(s) => s.size.clone(),
                SaftUintTy::U16(s) => s.size.clone(),
                SaftUintTy::U32(s) => s.size.clone(),
                SaftUintTy::U64(s) => s.size.clone(),
                SaftUintTy::U128(s) => s.size.clone(),
            }
        }
    }

    #[derive(Clone, Debug)]
    pub enum SaftFloatTy {
        F32(SaftPrimTySize<f32>),
        F64(SaftPrimTySize<f64>),
    }

    impl SaftFloatTy {
        pub fn collect_size(&self) -> SizeType {
            match self {
                SaftFloatTy::F32(s) => s.size.clone(),
                SaftFloatTy::F64(s) => s.size.clone(),
            }
        }
    }

    #[derive(Clone, Debug)]
    pub struct SaftPrimTySize<T>
// TODO: find a trait bound for primitive types
    {
        pub size: SizeType,
        _phantom: PhantomData<T>,
    }

    impl<T> SaftPrimTySize<T> {
        pub fn new() -> SaftPrimTySize<T> {
            SaftPrimTySize {
                // convert to number of bits
                size: SizeType::Concrete((mem::size_of::<T>() * BYTE).try_into().unwrap()),
                _phantom: PhantomData,
            }
        }
    }
}

pub mod bounded_vec_type {

    use super::{SizeType, ValueTypeV2};

    #[derive(Clone, Debug)]
    pub struct BoundedVec {
        pub ty: Box<ValueTypeV2>,
        //pub length_bound: GetTrait,
        pub size: SizeType,
    }

    impl BoundedVec {
        pub fn new(ty: ValueTypeV2 /*, max_length: Getu32Trait whatever*/) -> BoundedVec {
            BoundedVec {
                ty: Box::new(ty),
                size: SizeType::default(),
            }
        }
    }
}

pub mod tuple_type {
    use super::{SizeType, ValueTypeV2};

    #[derive(Clone, Debug)]
    pub struct TupleTy {
        pub types: Vec<Box<ValueTypeV2>>,
        pub size: SizeType,
    }

    impl TupleTy {
        pub fn new(types: Vec<ValueTypeV2>) -> TupleTy {
            TupleTy {
                types: types.iter().cloned().map(|e| Box::new(e)).collect(),
                size: SizeType::default(),
            }
        }
    }
}

pub mod array_type {
    use super::{CompositeSize, SizeType, ValueTypeV2};

    #[derive(Clone, Debug)]
    pub struct ArrayTy {
        pub ty: Box<ValueTypeV2>,
        pub size: SizeType,
    }

    impl ArrayTy {
        pub fn new(ty: ValueTypeV2, length: SizeType) -> ArrayTy {
            let sizes = vec![ty.collect_size()];
            ArrayTy {
                ty: Box::new(ty),
                size: SizeType::Composite(
                    // Size of the array is size of an element
                    // times length of the array
                    Box::new(CompositeSize {
                        mul_factor: length,
                        sizes,
                    }),
                ),
            }
        }
    }
}

pub mod slice_type {
    use super::{SizeType, ValueTypeV2};

    #[derive(Clone, Debug)]
    pub struct SliceTy {
        pub ty: Box<ValueTypeV2>,
        pub size: SizeType,
    }

    impl SliceTy {
        pub fn new(ty: ValueTypeV2, size: SizeType) -> SliceTy {
            SliceTy {
                ty: Box::new(ty),
                size,
            }
        }
    }
}

#[derive(Debug)]
pub enum TypeVariant {
    FrameStorageType(FrameStorageType),
    PalletDeclaredType(PalletDeclaredType),
    PalletConfigConstantType(PalletConfigConstantType),
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
        todo!();
        self.tsm.insert_mut(ty.get_name_full(tcx), ty);
    }
}

impl Default for TySys {
    fn default() -> Self {
        Self::new()
    }
}

pub fn explore(tcx: &TyCtxt, ty: &rustc_hir::Ty, ts: &TySys) -> ValueTypeV2 {
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
                            TypeVariant::PalletDeclaredType(t) => todo!(),
                            TypeVariant::PalletConfigConstantType(t) => todo!(),
                            TypeVariant::FrameStorageType(_) => unreachable!(),
                        }
                    } else {
                        // Simply return it as a symbol, TypeRelative paths may be resolved in later work
                        ValueTypeV2::Symbol {
                            symbol: super_type.to_owned() + "::" + segment.ident.as_str(),
                            size: SizeType::Symbolic(
                                super_type.to_owned() + "::" + segment.ident.as_str(),
                            ),
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
        rustc_hir::TyKind::Tup(tys) => {
            // Block for Tuple
            let mut members = Vec::new();

            for ty in tys.iter() {
                members.push(Box::new(explore(tcx, ty, ts)));
            }

            todo!() //ValueTypeV2::Tuple(members)
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
                                todo!();return ValueTypeV2::Array {
                                    value: Box::new(explore(tcx, ty, ts)),
                                    size: VecSize::Known(value)
                                };
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
        rustc_hir::TyKind::Slice(ty) => todo!(), /*ValueTypeV2::Slice(Box::new(explore(tcx, ty, ts)))*/
        rustc_hir::TyKind::BareFn(bare_fn_ty) => {
            let rustc_hir::BareFnTy { decl, .. } = bare_fn_ty;

            let mut inputs = Vec::new();

            for input_ty in decl.inputs.iter() {
                inputs.push(Box::new(explore(tcx, input_ty, ts)));
            }

            let output = match decl.output {
                rustc_hir::FnRetTy::DefaultReturn(_) => Box::new(ValueTypeV2::DefaultRet),
                rustc_hir::FnRetTy::Return(ty) => Box::new(explore(tcx, ty, ts)),
            };

            todo!();
            ValueTypeV2::Fn { inputs, output }
        }
        _ => {
            println!("{:?}", ty);
            unreachable!()
        }
    }
}

/// Find the ValueType enum member that corresponds to the given path
pub fn get_value_type(tcx: &TyCtxt, path: &rustc_hir::Path, ts: &TySys) -> ValueTypeV2 {
    let rustc_hir::Path { segments, res, .. } = path;

    match res {
        /*Res::Def(_, def_id) => {
            match get_def_id_name_with_path(*tcx, *def_id).as_str() {
                // BoundedVec is a standard type in FRAME:
                // https://docs.substrate.io/rustdocs/latest/frame_support/storage/bounded_vec/struct.BoundedVec.html
                "frame_support::BoundedVec" => {
                    let generics = segments[0].args.unwrap().args;
                    if let rustc_hir::GenericArg::Type(ty_0) = &generics[0]
                        && let rustc_hir::GenericArg::Type(ty_1) = &generics[1]
                    {
                        let value = Box::new(explore(tcx, ty_0, ts));
                        let size = match explore(tcx, ty_1, ts) {
                            ValueTypeV2::Usize => VecSize::Usize,
                            ValueTypeV2::U8 => VecSize::U8,
                            ValueTypeV2::U16 => VecSize::U16,
                            ValueTypeV2::U32 => VecSize::U32,
                            ValueTypeV2::U64 => VecSize::U64,
                            ValueType::U128 => VecSize::U128,
                            ValueType::Symbol(symbol) => VecSize::Symbol(symbol),
                            _ => todo!(),
                        };

                        return ValueType::BoundedVec { value, size };
                    }

                    unreachable!()
                }
                "std::option::Option" => {
                    let generic = &segments[0].args.unwrap().args[0];

                    if let rustc_hir::GenericArg::Type(ty) = generic {
                        return ValueTypeV2::Option(Box::new(explore(tcx, ty, ts)));
                    }

                    unreachable!()
                }
                "frame_support::traits::Get" => {
                    let generic = &segments[0].args.unwrap().args[0];

                    if let rustc_hir::GenericArg::Type(ty) = generic {
                        return ValueTypeV2::Get(Box::new(explore(tcx, ty, ts)));
                    }

                    unreachable!()
                }
                _ => {
                    //println!("{}", get_def_id_name_with_path(*tcx, *def_id).as_str());
                    // Treat every unkown as symbol, later work will maybe resolve those
                    let key = get_def_id_name_with_path(*tcx, *def_id);
                    if ts.tsm.contains_key(&key) {
                        match ts.tsm.get(&key).unwrap() {
                            TypeVariant::PalletDeclaredType(t) => t.value.clone(),
                            TypeVariant::PalletConfigConstantType(t) => t.trait_bound.clone(),
                            TypeVariant::FrameStorageType(_) => unreachable!(),
                        }
                    } else {
                        ValueTypeV2::Symbol(get_def_id_name(*tcx, *def_id))
                    }
                }
            }
        }*/
        // Primitive types
        // https://doc.rust-lang.org/reference/type-layout.html#primitive-data-layout
        Res::PrimTy(prim_ty) => match prim_ty {
            PrimTy::Int(int_ty) => match int_ty {
                //TODO: only have ValueType::Int(Isize)
                IntTy::Isize => ValueTypeV2::PrimTy(SaftPrimTy::Int(SaftIntTy::Isize(SaftPrimTySize::new()))),
                IntTy::I8 => ValueTypeV2::PrimTy(SaftPrimTy::Int(SaftIntTy::I8(SaftPrimTySize::new()))),
                IntTy::I16 => ValueTypeV2::PrimTy(SaftPrimTy::Int(SaftIntTy::I16(SaftPrimTySize::new()))),
                IntTy::I32 => ValueTypeV2::PrimTy(SaftPrimTy::Int(SaftIntTy::I32(SaftPrimTySize::new()))),
                IntTy::I64 => ValueTypeV2::PrimTy(SaftPrimTy::Int(SaftIntTy::I64(SaftPrimTySize::new()))),
                IntTy::I128 => ValueTypeV2::PrimTy(SaftPrimTy::Int(SaftIntTy::I128(SaftPrimTySize::new()))),
            },
            PrimTy::Uint(uint_ty) => match uint_ty {
                UintTy::Usize => ValueTypeV2::PrimTy(SaftPrimTy::Uint(SaftUintTy::Usize(SaftPrimTySize::new()))),
                UintTy::U8 => ValueTypeV2::PrimTy(SaftPrimTy::Uint(SaftUintTy::U8(SaftPrimTySize::new()))),
                UintTy::U16 => ValueTypeV2::PrimTy(SaftPrimTy::Uint(SaftUintTy::U16(SaftPrimTySize::new()))),
                UintTy::U32 => ValueTypeV2::PrimTy(SaftPrimTy::Uint(SaftUintTy::U32(SaftPrimTySize::new()))),
                UintTy::U64 => ValueTypeV2::PrimTy(SaftPrimTy::Uint(SaftUintTy::U64(SaftPrimTySize::new()))),
                UintTy::U128 => ValueTypeV2::PrimTy(SaftPrimTy::Uint(SaftUintTy::U128(SaftPrimTySize::new()))),
            },
            PrimTy::Float(float_ty) => match float_ty {
                FloatTy::F32 => ValueTypeV2::PrimTy(SaftPrimTy::Float(SaftFloatTy::F32(SaftPrimTySize::new()))),
                FloatTy::F64 => ValueTypeV2::PrimTy(SaftPrimTy::Float(SaftFloatTy::F64(SaftPrimTySize::new()))),
            },
            PrimTy::Str => ValueTypeV2::PrimTy(SaftPrimTy::Str(SaftPrimTySize::new())),
            PrimTy::Bool => ValueTypeV2::PrimTy(SaftPrimTy::Bool(SaftPrimTySize::new())),
            PrimTy::Char => ValueTypeV2::PrimTy(SaftPrimTy::Char(SaftPrimTySize::new())),
        },
        _ => todo!(),
    }
}
