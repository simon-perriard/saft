use crate::typesystem_common::*;

#[derive(Clone, Debug)]
pub struct Struct {
    pub variants: Vec<(Box<Type>, String)>,
    pub size: SizeType,
}

impl Struct {
    pub fn new(variants: Vec<(Type, String)>) -> Struct {
        let size = SizeType::Composite(Box::new(CompositeSize {
            mul_factor: SizeType::Concrete(1),
            sizes: variants.iter().map(|(e, _)| e.collect_size()).collect(),
        }));
        Struct {
            variants: variants.iter().cloned().map(|(e, f)| (Box::new(e), f)).collect(),
            size,
        }
    }
}
