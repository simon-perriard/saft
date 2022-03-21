use rpds::{HashTrieMap, HashTrieSet};
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
impl Size {
    /*pub fn add(&self, other: Size) -> Size {
        match self {
            Size::Concrete(x) => match other {
                Size::Concrete(y) => Size::Concrete(x + y),
                Size::Symbol(_) => unreachable!(),
                Size::Zero => Size::Concrete(*x),
                Size::One => Size::Concrete(x+1)
            },
            Size::Symbol(s) => match other {
                Size::Concrete(_) => unreachable!(),
                Size::Symbol(t) => {
                    assert!(s.eq(&t));
                    (*self).clone()
                }
                Size::Zero => (*self).clone(),
                _ => unreachable!(),
            },
            Size::One => {
                match other {
                    Size::One => Size::Concrete(2),
                    _ => other.add((*self).clone())
                }
            },
            Size::Zero => {
                match other {
                    Size::Zero => unreachable!(),
                    _ => other
                }
            },
        }
    }*/
}

#[derive(Default, Clone, Debug)]
pub struct CompSize {
    pub mul_factor: Size,
    pub symbols: HashTrieMap<String, Size>,
    pub concrete: usize,
}

impl CompSize {
    pub fn new_symbol(symbol: String) -> CompSize {
        let mut symbols = HashTrieMap::new();
        symbols.insert_mut(symbol, Size::One);
        CompSize {
            mul_factor: Size::One,
            symbols,
            concrete: 0,
        }
    }

    pub fn new_concrete(concrete: usize) -> CompSize {
        CompSize {
            mul_factor: Size::One,
            symbols: HashTrieMap::new(),
            concrete,
        }
    }

    pub fn set_mul_factor(&mut self, mul_factor: Size) {
        self.mul_factor = mul_factor;
    }
}

impl fmt::Display for CompSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fmt = String::new();

        let mut count = self.symbols.keys().len()-1;

        for (ty, size) in self.symbols.iter() {
            match size {
                Size::One => fmt.push_str(&format!("{}", ty)),
                Size::Zero => continue,
                _ => fmt.push_str(&format!("({} * {})", size, ty))
            }
            
            if count > 0 {
                fmt.push_str(" + ")
            }

            count +=1;
        }

        if self.concrete != 0 {
            fmt.push_str(&format!(" + {}", self.concrete));
        }

        match self.mul_factor {
            Size::Zero => {
                write!(f, "")
            },
            Size::One => {
                write!(f, "{}", fmt)
            },
            _ => {
                write!(f, "{} * ({})", self.mul_factor, fmt)
            }
        }
    }
}

/*impl std::ops::Add for CompSize {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        let mut keys = HashTrieSet::new();
        let mut symbols = HashTrieMap::new();

        for key in self.symbols.keys() {
            keys.insert_mut(key);
        }
        for key in other.symbols.keys() {
            keys.insert_mut(key);
        }

        for key in keys.iter() {
            let count_self = match self.symbols.get(*key) {
                Some(x) => *x,
                None => Size::Zero,
            };

            let count_other = match other.symbols.get(*key) {
                Some(x) => *x,
                None => Size::Zero,
            };

            symbols.insert_mut((*key).clone(), count_self.add(count_other));
        }

        Self {
            mul_factor:
            symbols,
            concrete: self.concrete + other.concrete,
        }
    }
}

impl std::ops::AddAssign for CompSize {
    fn add_assign(&mut self, other: Self) {
        for key in other.symbols.keys() {
            let count_self = match self.symbols.get(key) {
                Some(x) => *x,
                None => Size::Zero,
            };

            let count_other = match other.symbols.get(key) {
                Some(x) => *x,
                None => Size::Zero,
            };

            self.symbols
                .insert_mut((*key).clone(), count_other.add(count_self));
        }

        self.concrete += other.concrete;
    }
}*/

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
                let mut acc = CompSize::default();

                for t in t_vec.iter() {
                    //acc += t.get_size();
                }

                acc
            }
            // TODO multiply by size, which could be a symbol
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
