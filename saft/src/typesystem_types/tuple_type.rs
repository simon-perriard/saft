use crate::typesystem_common::*;

#[derive(Clone, Debug)]
pub struct Tuple {
    pub types: Vec<Box<Type>>,
    pub size: Size,
}

impl Tuple {
    pub fn new(types: Vec<Type>) -> Tuple {
        Tuple {
            types: types.iter().cloned().map(Box::new).collect(),
            size: fill_size(types),
        }
    }
}
