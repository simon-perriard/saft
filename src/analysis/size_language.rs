use core::fmt;

/*
single Size thing in a single enum wiht concrete, symbolic, unit, add, mul, max

have a linear multiplication Mul(u128, Size) for example

for bounded vec, have the size with TYPE::get
*/
use rustc_middle::ty::TyCtxt;

#[derive(Eq, PartialEq, Clone, Debug)]
pub(crate) enum Size {
    Concrete(u128),
    Symbolic(String, bool), // bool is to tell whether it has an associated function
    Unit,
    Add(Box<Size>, Box<Size>),
    Mul(Box<Size>, Box<Size>),
    LinMul(u128, Box<Size>),
    Max(Box<Size>, Box<Size>),
}

impl Default for Size {
    fn default() -> Self {
        Size::Unit
    }
}

impl Size {
    pub(crate) fn concrete(c: u128) -> Self {
        Self::Concrete(c)
    }

    pub(crate) fn symbolic(s: String, has_function: bool) -> Self {
        Self::Symbolic(s, has_function)
    }

    pub(crate) fn unit() -> Self {
        Self::Unit
    }

    pub(crate) fn is_zero(&self) -> bool {
        match self {
            Self::Concrete(x) => *x == 0,
            Self::Symbolic(_, _) => false,
            Self::Unit => true,
            Self::Add(a, b) => a.is_zero() && b.is_zero(),
            Self::Mul(a, b) => a.is_zero() || b.is_zero(),
            Self::LinMul(a, b) => *a == 0 || b.is_zero(),
            Self::Max(a, b) => a.is_zero() && b.is_zero(),
        }
    }

    pub(crate) fn max(&self, rhs: &Self) -> Size {
        if self.is_zero() {
            return rhs.clone();
        } else if rhs.is_zero() {
            return (*self).clone();
        } else if *self == *rhs {
            return (*self).clone();
        } else if let Size::Concrete(x) = *self && let Size::Concrete(y) = *rhs {
            // Compact notation: max(x, y) => x > y ? x : y
            return if x > y {(*self).clone()} else {rhs.clone()};
        } else if let Size::Max(a, _) = (*rhs).clone() && *self == *a {
            // Compact notation: max(a, max(a, b)) => max(a, b)
            return rhs.clone();
        }  else if let Size::Max(a, _) = (*self).clone() && *rhs == *a {
            // Compact notation: max(max(a,b), a) => max(a, b)
            return (*self).clone();
        } else if let Size::Max(_, b) = (*rhs).clone() && *self == *b {
            // Compact notation: max(b, max(a, b)) => max(a, b)
            return rhs.clone();
        } else if let Size::Max(_, b) = (*self).clone() && *rhs == *b {
            // Compact notation: max(max(a, b), b) => max(a, b)
            return (*self).clone();
        }

        // Try to resolve the max
        if let Some(longest) = self.cmp_add_chain(rhs) {
            return longest;
        }

        Self::Max(Box::new((*self).clone()), Box::new(rhs.clone()))
    }

    fn flatten_add_chain(&self) -> Vec<Self> {
        let mut flat = Vec::new();

        match self {
            Size::Add(a, b) => {
                flat.append(&mut a.flatten_add_chain());
                flat.append(&mut b.flatten_add_chain());
            }
            _ => flat.push(self.clone()),
        }

        flat
    }

    // Compare an Add or Size with another Add
    fn cmp_add_chain(&self, other: &Self) -> Option<Self> {
        let mut chain_1 = self.flatten_add_chain();
        let mut chain_2 = other.flatten_add_chain();

        // Remove common elements from chain_1
        chain_1.drain_filter(|size_1| {
            let res = chain_2.contains(size_1);
            if res {
                let chain_2_index = chain_2
                    .iter()
                    .position(|size_2| *size_1 == *size_2)
                    .unwrap();
                chain_2.remove(chain_2_index);
            }
            res
        });

        // We can say something only if at least one of them is empty,
        // otherwise cannot decide which has most cost

        if chain_1.is_empty() && chain_2.is_empty() {
            // Both chains have equal value
            Some((*other).clone())
        } else if chain_1.is_empty() {
            // Chain 2 is bigger
            Some((*other).clone())
        } else if chain_2.is_empty() {
            // Chain 1 is bigger
            Some((*self).clone())
        } else {
            None
        }
    }

    fn pretty_print_need_parenthesis(&self) -> bool {
        match self {
            Self::Add(_, _) => true,
            _ => false,
        }
    }
}

impl std::ops::Add for Size {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self.is_zero() {
            return rhs;
        } else if rhs.is_zero() {
            return self;
        } else if let Self::Concrete(x) = self && let Self::Concrete(y) = rhs {
            return Self::Concrete(x+y);
        }

        Self::Add(Box::new(self), Box::new(rhs))
    }
}

impl std::ops::Mul for Size {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        if self.is_zero() || rhs.is_zero() {
            return Self::unit();
        } else if let Self::Concrete(x) = self && let Self::Concrete(y) = rhs {
            return Self::Concrete(x*y);
        } else if let Self::Concrete(x) = self && let Self::LinMul(y, a) = rhs {
            return Self::LinMul(x*y, a);
        } else if let Self::Concrete(x) = rhs && let Self::LinMul(y, a) = self {
            return Self::LinMul(x*y, a);
        } else if let Self::Concrete(x) = self {
            return Self::LinMul(x, Box::new(rhs));
        } else if let Self::Concrete(x) = rhs {
            return Self::LinMul(x, Box::new(self));
        }

        Self::Mul(Box::new(self), Box::new(rhs))
    }
}

impl fmt::Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Concrete(x) => write!(f, "{}", x),
            Self::Symbolic(s, has_function) => {
                if *has_function {
                    write!(f, "VALUEOF({})", s)
                } else {
                    write!(f, "SIZEOF({})", s)
                }
            }
            Self::Unit => write!(f, ""),
            Self::Add(a, b) => write!(f, "{} + {}", a, b),
            Self::Mul(a, b) => {
                if a.pretty_print_need_parenthesis() && b.pretty_print_need_parenthesis() {
                    write!(f, "({}) * ({})", a, b)
                } else if a.pretty_print_need_parenthesis() {
                    write!(f, "({}) * {}", a, b)
                } else if b.pretty_print_need_parenthesis() {
                    write!(f, "{} * ({})", a, b)
                } else {
                    write!(f, "{} * {}", a, b)
                }
            }
            Self::LinMul(a, b) => write!(f, "{} * ({})", a, b),
            Self::Max(a, b) => write!(f, "MAX({}, {})", a, b),
        }
    }
}

pub(crate) trait HasSize {
    fn get_size(&self, tcx: &TyCtxt) -> Size;
}
