use crate::typesystem_common::*;

#[derive(Clone, Debug)]
pub struct Array {
    pub ty: Box<Type>,
    pub size: SizeType,
}

impl Array {
    pub fn new(ty: Type, length: SizeType) -> Array {
        let sizes = vec![ty.collect_size()];
        Array {
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
