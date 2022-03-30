use std::{marker::PhantomData, mem};

use crate::typesystem_common::SizeType;

const BYTE: usize = 8;

#[derive(Clone, Debug)]
pub enum Primitive {
    Int(Int),
    Uint(Uint),
    Float(Float),
    Str(PrimitiveSize<&'static str>),
    Bool(PrimitiveSize<bool>),
    Char(PrimitiveSize<char>),
}

impl Primitive {
    pub fn collect_size(&self) -> SizeType {
        match self {
            Primitive::Int(ty) => ty.collect_size(),
            Primitive::Uint(ty) => ty.collect_size(),
            Primitive::Float(ty) => ty.collect_size(),
            Primitive::Str(s) => s.size.clone(),
            Primitive::Bool(s) => s.size.clone(),
            Primitive::Char(s) => s.size.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Int {
    Isize(PrimitiveSize<isize>),
    I8(PrimitiveSize<i8>),
    I16(PrimitiveSize<i16>),
    I32(PrimitiveSize<i32>),
    I64(PrimitiveSize<i64>),
    I128(PrimitiveSize<i128>),
}

impl Int {
    pub fn collect_size(&self) -> SizeType {
        match self {
            Int::Isize(s) => s.size.clone(),
            Int::I8(s) => s.size.clone(),
            Int::I16(s) => s.size.clone(),
            Int::I32(s) => s.size.clone(),
            Int::I64(s) => s.size.clone(),
            Int::I128(s) => s.size.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Uint {
    Usize(PrimitiveSize<usize>),
    U8(PrimitiveSize<u8>),
    U16(PrimitiveSize<u16>),
    U32(PrimitiveSize<u32>),
    U64(PrimitiveSize<u64>),
    U128(PrimitiveSize<u128>),
}

impl Uint {
    pub fn collect_size(&self) -> SizeType {
        match self {
            Uint::Usize(s) => s.size.clone(),
            Uint::U8(s) => s.size.clone(),
            Uint::U16(s) => s.size.clone(),
            Uint::U32(s) => s.size.clone(),
            Uint::U64(s) => s.size.clone(),
            Uint::U128(s) => s.size.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Float {
    F32(PrimitiveSize<f32>),
    F64(PrimitiveSize<f64>),
}

impl Float {
    pub fn collect_size(&self) -> SizeType {
        match self {
            Float::F32(s) => s.size.clone(),
            Float::F64(s) => s.size.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PrimitiveSize<T>
// TODO: find a trait bound for primitive types
{
    pub size: SizeType,
    _phantom: PhantomData<T>,
}

impl<T> PrimitiveSize<T> {
    pub fn new() -> PrimitiveSize<T> {
        PrimitiveSize {
            // convert to number of bits
            size: SizeType::Concrete((mem::size_of::<T>() * BYTE).try_into().unwrap()),
            _phantom: PhantomData,
        }
    }
}

impl<T> Default for PrimitiveSize<T> {
    fn default() -> Self {
        Self::new()
    }
}
