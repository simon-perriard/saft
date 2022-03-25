//! Basic elements to build the extended typesystem

use rpds::HashTrieMap;
use rustc_ast::ast::{FloatTy, IntTy, UintTy};
use rustc_hir::def::Res;
use rustc_hir::def_id::DefId;
use rustc_hir::PrimTy;
use rustc_middle::mir::interpret;
use rustc_middle::ty::TyCtxt;
use std::fmt;
use std::mem;

use crate::analysis_utils::def_id_printer::*;
use crate::typesystem_config_constant_types::PalletConfigConstantType;
use crate::typesystem_declared_types::PalletDeclaredType;
use crate::typesystem_storage::FrameStorageType;

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
pub enum Size {
    Concrete(u128),
    Symbolic(String),
    //TODO: remove ZEro and One
    Zero,
    One,
}

impl Default for Size {
    fn default() -> Self {
        Size::Zero
    }
}

impl fmt::Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Size::Concrete(x) => write!(f, "{}", x),
            Size::Symbolic(s) => write!(f, "{}", s),
            Size::One => write!(f, "1"),
            Size::Zero => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
/// A composite size, it can contain different elements to describe the size of a type
pub struct CompSize {
    /// Multiplication factor associated to the size,
    /// this can be used for an array that has a max size.
    /// Example:
    ///     BoundedVec<u8, MAX::Type>
    ///     will be represented by CompSize as
    ///     mul_factor: MAX::Type'size and concrete 1 (byte)
    pub mul_factor: Size,
    /// The symbols hashmap represents user defined types,
    /// for which we may (concrete) or may not (symbol) know the size.
    pub symbols: HashTrieMap<String, Size>,
    /// The sum of type' sizes we know for sure
    pub concrete: usize,
    /// This is little hack to represent Tuples and the size
    /// of each of their members.
    pub tuple_composition: Option<Vec<Box<CompSize>>>,
}

impl Default for CompSize {
    fn default() -> Self {
        Self {
            mul_factor: Size::One,
            symbols: Default::default(),
            concrete: Default::default(),
            tuple_composition: Default::default(),
        }
    }
}

impl CompSize {
    /// Create a new symbolic size
    pub fn new_symbol(symbol: String) -> CompSize {
        let mut symbols = HashTrieMap::new();
        symbols.insert_mut(symbol, Size::One);
        CompSize {
            mul_factor: Size::One,
            symbols,
            concrete: 0,
            tuple_composition: None,
        }
    }

    /// Create a new concrete size
    pub fn new_concrete(concrete: usize) -> CompSize {
        CompSize {
            mul_factor: Size::One,
            symbols: HashTrieMap::new(),
            concrete,
            tuple_composition: None,
        }
    }

    pub fn set_mul_factor(&mut self, mul_factor: Size) {
        self.mul_factor = mul_factor;
    }
}

impl fmt::Display for CompSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fmt = String::new();

        let mut count = self.symbols.keys().len() - 1;

        for (ty, size) in self.symbols.iter() {
            match size {
                Size::One => fmt.push_str(ty),
                Size::Zero => continue,
                _ => fmt.push_str(&format!("({} * {})", size, ty)),
            }

            if count > 0 {
                fmt.push_str(" + ")
            }
            count -= 1;
        }

        if let Some(tuple_composition) = &self.tuple_composition {
            count = tuple_composition.len() - 1;
            fmt.push('(');
            for member in tuple_composition.iter() {
                fmt.push_str(&format!("{}", member));

                if count > 0 {
                    fmt.push_str(" + ")
                }
                count -= 1;
            }
            fmt.push(')');
        }

        if self.concrete != 0 {
            if !fmt.is_empty() {
                fmt.push_str(" + ");
            }
            fmt.push_str(&format!("{}", self.concrete));
        }

        match self.mul_factor {
            Size::Zero => {
                write!(f, "")
            }
            Size::One => {
                write!(f, "{}", fmt)
            }
            _ => {
                write!(f, "{} * ({})", self.mul_factor, fmt)
            }
        }
    }
}

#[derive(Clone, Debug)]
/// Only types that can represent a size (vector size)
pub enum VecSize {
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    Symbol(String),
    Known(u128),
}

impl VecSize {
    pub fn get_size(&self) -> Size {
        match self {
            VecSize::U8 => Size::Concrete(mem::size_of::<u8>().try_into().unwrap()),
            VecSize::U16 => Size::Concrete(mem::size_of::<u16>().try_into().unwrap()),
            VecSize::U32 => Size::Concrete(mem::size_of::<u32>().try_into().unwrap()),
            VecSize::U64 => Size::Concrete(mem::size_of::<u64>().try_into().unwrap()),
            VecSize::U128 => Size::Concrete(mem::size_of::<u128>().try_into().unwrap()),
            VecSize::Usize => Size::Concrete(mem::size_of::<usize>().try_into().unwrap()),
            VecSize::Symbol(s) => Size::Symbolic((*s).clone()),
            VecSize::Known(x) => Size::Concrete(*x),
        }
    }
}

#[derive(Clone, Debug)]
/// Different types of values, unknown types should be
/// defferred to Symbol
pub enum ValueType {
    // TODO:
    // have is as Type(arg1, arg2, ... ,TypeSize)
    // example: BoundedVec(Box<ValueType>, GetU32Type, BoundedVecSize)
    // GetU32Type as a struct with an identifier
    // GetU32Type contains the identifier of the rustc type passed as second arg to the boundedvec
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
    F32,
    F64,
    Str,
    Bool,
    Char,
    Option(Box<ValueType>),
    Tuple(Vec<Box<ValueType>>),
    BoundedVec {
        value: Box<ValueType>,
        size: VecSize,
        //TODO: add extra fields with size attributes
    },
    Array {
        value: Box<ValueType>,
        size: VecSize,
    },
    Slice(Box<ValueType>),
    Get(Box<ValueType>),
    Symbol(String),
    Fn {
        inputs: Vec<Box<ValueType>>,
        output: Box<ValueType>,
    },
    DefaultRet,
}

impl ValueType {
    pub fn get_size(&self) -> CompSize {
        // https://doc.rust-lang.org/reference/type-layout.html
        match self {
            ValueType::U8 => CompSize::new_concrete(mem::size_of::<u8>()),
            ValueType::U16 => CompSize::new_concrete(mem::size_of::<u16>()),
            ValueType::U32 => CompSize::new_concrete(mem::size_of::<u32>()),
            ValueType::U64 => CompSize::new_concrete(mem::size_of::<u64>()),
            ValueType::U128 => CompSize::new_concrete(mem::size_of::<u128>()),
            ValueType::Usize => CompSize::new_concrete(mem::size_of::<usize>()),
            ValueType::I8 => CompSize::new_concrete(mem::size_of::<i8>()),
            ValueType::I16 => CompSize::new_concrete(mem::size_of::<i16>()),
            ValueType::I32 => CompSize::new_concrete(mem::size_of::<i32>()),
            ValueType::I64 => CompSize::new_concrete(mem::size_of::<i64>()),
            ValueType::I128 => CompSize::new_concrete(mem::size_of::<i128>()),
            ValueType::Isize => CompSize::new_concrete(mem::size_of::<isize>()),
            ValueType::F32 => CompSize::new_concrete(mem::size_of::<f32>()),
            ValueType::F64 => CompSize::new_concrete(mem::size_of::<f64>()),
            ValueType::Str => CompSize::new_concrete(mem::size_of::<&str>()),
            ValueType::Bool => CompSize::new_concrete(mem::size_of::<bool>()),
            ValueType::Char => CompSize::new_concrete(mem::size_of::<char>()),
            ValueType::Option(t) => t.get_size(),
            ValueType::Tuple(t_vec) => {
                let mut tuple_vec = Vec::new();

                for t in t_vec.iter() {
                    tuple_vec.push(Box::new(t.get_size()));
                }

                CompSize {
                    tuple_composition: Some(tuple_vec),
                    ..CompSize::default()
                }
            }
            ValueType::BoundedVec { value, size } | ValueType::Array { value, size } => {
                let mut val = value.get_size();
                val.set_mul_factor(size.get_size());
                val
            }
            ValueType::Slice(t) => t.get_size(),
            ValueType::Get(t) => t.get_size(),
            ValueType::Symbol(symbol) => CompSize::new_symbol(symbol.to_string()),
            ValueType::Fn { output, .. } => output.get_size(),
            ValueType::DefaultRet => CompSize::default(),
        }
    }
}

/// Explore a type to get its path resolution and fill its
/// generics type arguments
pub fn explore(tcx: &TyCtxt, ty: &rustc_hir::Ty, ts: &TySys) -> ValueType {
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
                            TypeVariant::PalletDeclaredType(t) => t.value.clone(),
                            TypeVariant::PalletConfigConstantType(t) => t.trait_bound.clone(),
                            TypeVariant::FrameStorageType(_) => unreachable!(),
                        }
                    } else {
                        // Simply return it as a symbol, TypeRelative paths may be resolved in later work
                        ValueType::Symbol(super_type.to_owned() + "::" + segment.ident.as_str())
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

            ValueType::Tuple(members)
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
                                return ValueType::Array {
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
        rustc_hir::TyKind::Slice(ty) => ValueType::Slice(Box::new(explore(tcx, ty, ts))),
        rustc_hir::TyKind::BareFn(bare_fn_ty) => {
            let rustc_hir::BareFnTy { decl, .. } = bare_fn_ty;

            let mut inputs = Vec::new();

            for input_ty in decl.inputs.iter() {
                inputs.push(Box::new(explore(tcx, input_ty, ts)));
            }

            let output = match decl.output {
                rustc_hir::FnRetTy::DefaultReturn(_) => Box::new(ValueType::DefaultRet),
                rustc_hir::FnRetTy::Return(ty) => Box::new(explore(tcx, ty, ts)),
            };

            ValueType::Fn { inputs, output }
        }
        _ => {
            println!("{:?}", ty);
            unreachable!()
        }
    }
}

/// Find the ValueType enum member that correspond to the given path
pub fn get_value_type(tcx: &TyCtxt, path: &rustc_hir::Path, ts: &TySys) -> ValueType {
    let rustc_hir::Path { segments, res, .. } = path;

    match res {
        Res::Def(_, def_id) => {
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
                            ValueType::Usize => VecSize::Usize,
                            ValueType::U8 => VecSize::U8,
                            ValueType::U16 => VecSize::U16,
                            ValueType::U32 => VecSize::U32,
                            ValueType::U64 => VecSize::U64,
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
                        return ValueType::Option(Box::new(explore(tcx, ty, ts)));
                    }

                    unreachable!()
                }
                "frame_support::traits::Get" => {
                    let generic = &segments[0].args.unwrap().args[0];

                    if let rustc_hir::GenericArg::Type(ty) = generic {
                        return ValueType::Get(Box::new(explore(tcx, ty, ts)));
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
                        ValueType::Symbol(get_def_id_name(*tcx, *def_id))
                    }
                }
            }
        }
        // Primitive types
        // https://doc.rust-lang.org/reference/type-layout.html#primitive-data-layout
        Res::PrimTy(prim_ty) => match prim_ty {
            PrimTy::Int(int_ty) => match int_ty {
                //TODO: only have ValueType::Int(Isize)
                IntTy::Isize => ValueType::Isize,
                IntTy::I8 => ValueType::I8,
                IntTy::I16 => ValueType::I16,
                IntTy::I32 => ValueType::I32,
                IntTy::I64 => ValueType::I64,
                IntTy::I128 => ValueType::I128,
            },
            PrimTy::Uint(uint_ty) => match uint_ty {
                UintTy::Usize => ValueType::Usize,
                UintTy::U8 => ValueType::U8,
                UintTy::U16 => ValueType::U16,
                UintTy::U32 => ValueType::U32,
                UintTy::U64 => ValueType::U64,
                UintTy::U128 => ValueType::U128,
            },
            PrimTy::Float(float_ty) => match float_ty {
                FloatTy::F32 => ValueType::F32,
                FloatTy::F64 => ValueType::F64,
            },
            PrimTy::Str => ValueType::Str,
            PrimTy::Bool => ValueType::Bool,
            PrimTy::Char => ValueType::Char,
        },
        _ => todo!(),
    }
}

pub trait TypeSize {
    fn get_size(&self) -> CompSize;
    fn get_name(&self, tcx: &TyCtxt) -> String;
    fn get_name_full(&self, tcx: &TyCtxt) -> String;
}

#[derive(Debug)]
pub enum TypeVariant {
    FrameStorageType(FrameStorageType),
    PalletDeclaredType(PalletDeclaredType),
    PalletConfigConstantType(PalletConfigConstantType),
}

impl TypeSize for TypeVariant {
    fn get_size(&self) -> CompSize {
        match self {
            TypeVariant::FrameStorageType(t) => t.get_size(),
            TypeVariant::PalletDeclaredType(t) => t.get_size(),
            TypeVariant::PalletConfigConstantType(t) => t.get_size(),
        }
    }

    fn get_name(&self, tcx: &TyCtxt) -> String {
        match self {
            TypeVariant::FrameStorageType(t) => t.get_name(tcx),
            TypeVariant::PalletDeclaredType(t) => t.get_name(tcx),
            TypeVariant::PalletConfigConstantType(t) => t.get_name(tcx),
        }
    }

    fn get_name_full(&self, tcx: &TyCtxt) -> String {
        match self {
            TypeVariant::FrameStorageType(t) => t.get_name_full(tcx),
            TypeVariant::PalletDeclaredType(t) => t.get_name_full(tcx),
            TypeVariant::PalletConfigConstantType(t) => t.get_name_full(tcx),
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

    pub fn print_types_names(&self) {
        for key in self.tsm.keys() {
            println!("{}", key)
        }
    }

    pub fn print_types_with_sizes(&self) {
        for (name, t) in self.tsm.iter() {
            println!("{} - {}", name, t.get_size());
        }
    }

    pub fn print_types_hierarchies(&self) {
        for (name, t) in self.tsm.iter() {
            println!("{} - {:?}", name, t);
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
