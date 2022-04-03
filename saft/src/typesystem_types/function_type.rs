use crate::typesystem_common::*;

#[derive(Clone, Debug)]
pub struct Function {
    pub inputs: Vec<Box<Type>>,
    pub output: Box<Type>,
    pub size: Size,
}

impl Function {
    pub fn new(inputs: Vec<Type>, output: Type) -> Function {
        let size = output.collect_size();
        Function {
            inputs: inputs.iter().cloned().map(Box::new).collect(),
            output: Box::new(output),
            size,
        }
    }
}
