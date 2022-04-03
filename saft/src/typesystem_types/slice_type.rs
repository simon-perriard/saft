use crate::typesystem_common::*;

#[derive(Clone, Debug)]
pub struct Slice {
    pub ty: Box<Type>,
    pub size: Size,
}

impl Slice {
    pub fn new(ty: Type) -> Slice {
        let ty_size = ty.collect_size();
        Slice {
            ty: Box::new(ty),
            size: Size::Operation(Box::new(Operation::Mul(
                ty_size,
                Size::UnitSize(Box::new(todo!())),
            ))),
        }
    }
}
