use crate::typesystem_common::*;

#[derive(Clone, Debug)]
pub struct Slice {
    pub ty: Box<Type>,
    pub size: SizeType,
}

impl Slice {
    pub fn new(ty: Type) -> Slice {
        let sizes = vec![ty.collect_size()];
        Slice {
            ty: Box::new(ty),
            size: SizeType::Composite(Box::new(CompositeSize {
                mul_factor: SizeType::Symbolic("UNKNOWN".to_string()),
                sizes,
            })),
        }
    }
}
