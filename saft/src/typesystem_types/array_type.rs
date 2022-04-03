use crate::typesystem_common::*;

#[derive(Clone, Debug)]
pub struct Array {
    pub ty: Box<Type>,
    pub size: Size,
}

impl Array {
    pub fn new(ty: Type, length: Size) -> Array {
        let ty_size = ty.collect_size();
        Array {
            ty: Box::new(ty),
            size: Size::Operation(Box::new(Operation::Mul(ty_size, length))),
        }
    }
}
