use crate::typesystem_common::*;

#[derive(Clone, Debug)]
pub struct Tuple {
    pub types: Vec<Box<Type>>,
    pub size: SizeType,
}

impl Tuple {
    pub fn new(types: Vec<Type>) -> Tuple {
        let size = SizeType::Composite(Box::new(CompositeSize {
            mul_factor: SizeType::Concrete(1),
            sizes: types.iter().map(|e| e.collect_size()).collect(),
        }));
        Tuple {
            types: types.iter().cloned().map(Box::new).collect(),
            size,
        }
    }
}
