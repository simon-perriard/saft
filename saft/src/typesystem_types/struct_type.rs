use crate::{size_language::Size, typesystem_common::*};

#[derive(Clone, Debug)]
pub struct Struct {
    pub variants: Vec<(Box<Type>, String)>,
    pub size: Size,
}

impl Struct {
    pub fn new(variants: Vec<(Type, String)>) -> Struct {
        Struct {
            variants: variants
                .iter()
                .cloned()
                .map(|(e, f)| (Box::new(e), f))
                .collect(),
            size: fill_size(variants.iter().map(|(e, _)| e.clone()).collect()),
        }
    }
}
