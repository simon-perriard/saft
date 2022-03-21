use rpds::HashTrieMap;
use std::fmt;
use std::mem;

#[derive(Debug, Clone)]
pub enum Size {
    Concrete(usize),
    Symbol(String),
    Zero,
    One,
}

impl Default for Size {
    fn default() -> Self {
        Size::Zero
    }
}

impl fmt::Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Size::Concrete(x) => write!(f, "{}", x),
            Size::Symbol(s) => write!(f, "{}", s),
            Size::One => write!(f, "1"),
            Size::Zero => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CompSize {
    pub mul_factor: Size,
    pub symbols: HashTrieMap<String, Size>,
    pub concrete: usize,
    pub tuple_composition: Option<Vec<Box<CompSize>>>,
}

impl Default for CompSize {
    fn default() -> Self {
        Self {
            mul_factor: Size::One,
            symbols: Default::default(),
            concrete: Default::default(),
            tuple_composition: Default::default(),
        }
    }
}

impl CompSize {
    pub fn new_symbol(symbol: String) -> CompSize {
        let mut symbols = HashTrieMap::new();
        symbols.insert_mut(symbol, Size::One);
        CompSize {
            mul_factor: Size::One,
            symbols,
            concrete: 0,
            tuple_composition: None,
        }
    }

    pub fn new_concrete(concrete: usize) -> CompSize {
        CompSize {
            mul_factor: Size::One,
            symbols: HashTrieMap::new(),
            concrete,
            tuple_composition: None,
        }
    }

    pub fn set_mul_factor(&mut self, mul_factor: Size) {
        self.mul_factor = mul_factor;
    }
}

impl fmt::Display for CompSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fmt = String::new();

        let mut count = self.symbols.keys().len() - 1;

        for (ty, size) in self.symbols.iter() {
            match size {
                Size::One => fmt.push_str(ty),
                Size::Zero => continue,
                _ => fmt.push_str(&format!("({} * {})", size, ty)),
            }

            if count > 0 {
                fmt.push_str(" + ")
            }
            count -= 1;
        }

        if let Some(tuple_composition) = &self.tuple_composition {
            count = tuple_composition.len() - 1;
            fmt.push('(');
            for member in tuple_composition.iter() {
                fmt.push_str(&format!("{}", member));

                if count > 0 {
                    fmt.push_str(" + ")
                }
                count -= 1;
            }
            fmt.push(')');
        }

        if self.concrete != 0 {
            if !fmt.is_empty() {
                fmt.push_str(" + ");
            }
            fmt.push_str(&format!("{}", self.concrete));
        }

        match self.mul_factor {
            Size::Zero => {
                write!(f, "")
            }
            Size::One => {
                write!(f, "{}", fmt)
            }
            _ => {
                write!(f, "{} * ({})", self.mul_factor, fmt)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum VecSize {
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    Symbol(String),
}

impl VecSize {
    pub fn get_size(&self) -> Size {
        match self {
            VecSize::U8 => Size::Concrete(mem::size_of::<u8>()),
            VecSize::U16 => Size::Concrete(mem::size_of::<u16>()),
            VecSize::U32 => Size::Concrete(mem::size_of::<u32>()),
            VecSize::U64 => Size::Concrete(mem::size_of::<u64>()),
            VecSize::U128 => Size::Concrete(mem::size_of::<u128>()),
            VecSize::Usize => Size::Concrete(mem::size_of::<usize>()),
            VecSize::Symbol(s) => Size::Symbol((*s).clone()),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ValueType {
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,
    F32,
    F64,
    Str,
    Bool,
    Char,
    Option(Box<ValueType>),
    Tuple(Vec<Box<ValueType>>),
    BoundedVec {
        value: Box<ValueType>,
        size: VecSize,
    },
    Get(Box<ValueType>),
    Symbol(String),
}

impl ValueType {
    pub fn get_size(&self) -> CompSize {
        // https://doc.rust-lang.org/reference/type-layout.html
        match self {
            ValueType::U8 => CompSize::new_concrete(mem::size_of::<u8>()),
            ValueType::U16 => CompSize::new_concrete(mem::size_of::<u16>()),
            ValueType::U32 => CompSize::new_concrete(mem::size_of::<u32>()),
            ValueType::U64 => CompSize::new_concrete(mem::size_of::<u64>()),
            ValueType::U128 => CompSize::new_concrete(mem::size_of::<u128>()),
            ValueType::Usize => CompSize::new_concrete(mem::size_of::<usize>()),
            ValueType::I8 => CompSize::new_concrete(mem::size_of::<i8>()),
            ValueType::I16 => CompSize::new_concrete(mem::size_of::<i16>()),
            ValueType::I32 => CompSize::new_concrete(mem::size_of::<i32>()),
            ValueType::I64 => CompSize::new_concrete(mem::size_of::<i64>()),
            ValueType::I128 => CompSize::new_concrete(mem::size_of::<i128>()),
            ValueType::Isize => CompSize::new_concrete(mem::size_of::<isize>()),
            ValueType::F32 => CompSize::new_concrete(mem::size_of::<f32>()),
            ValueType::F64 => CompSize::new_concrete(mem::size_of::<f64>()),
            ValueType::Str => CompSize::new_concrete(mem::size_of::<&str>()),
            ValueType::Bool => CompSize::new_concrete(mem::size_of::<bool>()),
            ValueType::Char => CompSize::new_concrete(mem::size_of::<char>()),
            ValueType::Option(t) => t.get_size(),
            ValueType::Tuple(t_vec) => {
                let mut tuple_vec = Vec::new();

                for t in t_vec.iter() {
                    tuple_vec.push(Box::new(t.get_size()));
                }

                CompSize {
                    tuple_composition: Some(tuple_vec),
                    ..CompSize::default()
                }
            }
            ValueType::BoundedVec { value, size } => {
                let mut val = value.get_size();
                val.set_mul_factor(size.get_size());
                val
            }
            ValueType::Get(t) => t.get_size(),
            ValueType::Symbol(symbol) => CompSize::new_symbol(symbol.to_string()),
        }
    }
}
