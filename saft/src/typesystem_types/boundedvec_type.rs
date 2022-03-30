use crate::typesystem_common::Trait;
use crate::typesystem_common::*;

use super::primitive_types::*;

#[derive(Clone, Debug)]
pub struct BoundedVec {
    pub ty: Box<Type>,
    pub length_bound: Box<Trait>,
    pub size: SizeType,
}

impl BoundedVec {
    pub fn new(ty: Type) -> BoundedVec {
        let length_bound = Box::new(Trait::Get(Type::Primitive(Primitive::Uint(Uint::U32(
            PrimitiveSize::new(),
        )))));
        let size = SizeType::Composite(Box::new(CompositeSize {
            mul_factor: PrimitiveSize::<u32>::new().size,
            sizes: vec![ty.collect_size()],
        }));
        BoundedVec {
            ty: Box::new(ty),
            length_bound,
            size,
        }
    }
}
