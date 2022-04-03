use crate::typesystem_common::Trait;
use crate::typesystem_common::*;

use super::primitive_types::*;

#[derive(Clone, Debug)]
pub struct BoundedVec {
    pub ty: Box<Type>,
    pub length_bound: Box<Trait>,
    pub size: Size,
}

impl BoundedVec {
    pub fn new(ty: Type, max_length: Type) -> BoundedVec {
        let length_bound = Box::new(Trait::Get(Type::Primitive(Primitive::Uint(Uint::U32(
            PrimitiveSize::new(),
        )))));

        let length_interval = UnitSize::Interval(
            Size::UnitSize(Box::new(UnitSize::Concrete(0))),
            max_length.collect_size(),
        );
        let size = Size::Operation(Box::new(Operation::Mul(
            ty.collect_size(),
            Size::UnitSize(Box::new(length_interval)),
        )));

        BoundedVec {
            ty: Box::new(ty),
            length_bound,
            size,
        }
    }
}
