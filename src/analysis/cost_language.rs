use core::fmt;
use rustc_middle::ty::TyCtxt;

#[derive(Eq, PartialEq, Clone, Debug)]
pub(crate) enum Cost {
    Concrete(u64),
    Symbolic(Symbolic),
    Add(Box<Cost>, Box<Cost>),
    ConcreteMul(u64, Box<Cost>),
    SymbolicMul(Symbolic, Box<Cost>),
    Max(Box<Cost>, Box<Cost>),
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub(crate) enum Symbolic {
    ValueOf(String),
    SizeOf(String),
    TimeOf(String),
}

impl Symbolic {
    pub(crate) fn symbolic_mul(self, rhs: Cost) -> Cost {
        Cost::SymbolicMul(self, Box::new(rhs))
    }
}

impl fmt::Display for Symbolic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbolic::ValueOf(s) => write!(f, "VALUEOF({})", s),
            Symbolic::SizeOf(s) => write!(f, "SIZEOF({})", s),
            Symbolic::TimeOf(s) => write!(f, "TIMEOF({})", s),
        }
    }
}

impl Default for Cost {
    fn default() -> Self {
        Cost::Concrete(0)
    }
}

impl Cost {
    pub(crate) fn is_zero(&self) -> bool {
        match self {
            Self::Concrete(x) => *x == 0,
            Self::Symbolic(_) => false,
            Self::Add(a, b) => a.is_zero() && b.is_zero(),
            Self::SymbolicMul(_, b) => b.is_zero(),
            Self::ConcreteMul(a, b) => *a == 0 || b.is_zero(),
            Self::Max(a, b) => a.is_zero() && b.is_zero(),
        }
    }

    pub(crate) fn concrete_mul(self, rhs: Self) -> Self {
        if self.is_zero() || rhs.is_zero() {
            // Compact notation: x*y with x=0 and/or y=0 -> 0 
            return Self::Concrete(0);
        } else if let Self::Concrete(x) = self && let Self::Concrete(y) = rhs {
            // Compact notation: (x)*(y) with x and y concrete -> (x*y)
            return Self::Concrete(x*y);
        } else if let Self::Concrete(x) = self && let Self::ConcreteMul(y, a) = rhs {
            // Compact notation: (x)*((y)*a) with x and y concrete -> (x*y)*a
            return Self::ConcreteMul(x*y, a);
        } else if let Self::Concrete(x) = rhs && let Self::ConcreteMul(y, a) = self {
            // Compact notation: ((y)*a)*(x) with x and y concrete -> (x*y)*a
            return Self::ConcreteMul(x*y, a);
        } else if let Self::Concrete(x) = self {
            return Self::ConcreteMul(x, Box::new(rhs));
        } else if let Self::Concrete(x) = rhs {
            return Self::ConcreteMul(x, Box::new(self));
        }
        unreachable!();
    }

    pub(crate) fn max(&self, rhs: &Self) -> Self {
        if self.is_zero() {
            return rhs.clone();
        } else if rhs.is_zero() || *self == *rhs {
            return (*self).clone();
        } else if let Self::Concrete(x) = *self && let Self::Concrete(y) = *rhs {
            // Compact notation: max(x, y) => x > y ? x : y
            return if x > y {(*self).clone()} else {rhs.clone()};
        } else if let Self::Max(a, _) = (*rhs).clone() && *self == *a {
            // Compact notation: max(a, max(a, b)) => max(a, b)
            return rhs.clone();
        }  else if let Self::Max(a, _) = (*self).clone() && *rhs == *a {
            // Compact notation: max(max(a,b), a) => max(a, b)
            return (*self).clone();
        } else if let Self::Max(_, b) = (*rhs).clone() && *self == *b {
            // Compact notation: max(b, max(a, b)) => max(a, b)
            return rhs.clone();
        } else if let Self::Max(_, b) = (*self).clone() && *rhs == *b {
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
            Self::Add(a, b) => {
                flat.append(&mut a.flatten_add_chain());
                flat.append(&mut b.flatten_add_chain());
            }
            _ => flat.push(self.clone()),
        }

        flat
    }

    // Compare an Add or Self with another Add
    fn cmp_add_chain(&self, other: &Self) -> Option<Self> {
        let mut chain_1 = self.flatten_add_chain();
        let mut chain_2 = other.flatten_add_chain();

        // Remove common elements from chain_1
        chain_1.drain_filter(|cost_1| {
            let res = chain_2.contains(cost_1);
            if res {
                let chain_2_index = chain_2
                    .iter()
                    .position(|cost_2| *cost_1 == *cost_2)
                    .unwrap();
                chain_2.remove(chain_2_index);
            }
            res
        });

        // We can say something only if at least one of them is empty,
        // otherwise cannot decide which has most cost

        if chain_1.is_empty() && chain_2.is_empty() || chain_1.is_empty() {
            // Both chains have equal value OR
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
        matches!(self, Self::Add(_, _))
    }
}

impl std::ops::Add for Cost {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self.is_zero() {
            return rhs;
        } else if rhs.is_zero() {
            return self;
        } else if let Self::Concrete(x) = self && let Self::Concrete(y) = rhs {
            return Self::Concrete(x+y);
        } else if let Self::ConcreteMul(x, a) = self.clone() && *a == rhs {
            return Self::ConcreteMul(x+1, a);
        } else if let Self::ConcreteMul(x, a) = rhs.clone() && *a == self {
            return Self::ConcreteMul(x+1, a);
        } else if let Self::ConcreteMul(x, a) = self.clone() && let Self::ConcreteMul(y, b) = rhs.clone() && *a == *b{
            return Self::ConcreteMul(x+y, a);
        }

        Self::Add(Box::new(self), Box::new(rhs))
    }
}

impl fmt::Display for Cost {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Concrete(x) => write!(f, "{}", x),
            Self::Symbolic(symbolic) => write!(f, "{}", symbolic),
            Self::Add(a, b) => write!(f, "{} + {}", a, b),
            Self::SymbolicMul(a, b) => {
                if b.pretty_print_need_parenthesis() {
                    write!(f, "{} * ({})", a, b)
                } else {
                    write!(f, "{} * {}", a, b)
                }
            }
            Self::ConcreteMul(a, b) => write!(f, "{} * ({})", a, b),
            Self::Max(a, b) => write!(f, "MAX({}, {})", a, b),
        }
    }
}

pub(crate) trait HasSize {
    fn get_size(&self, tcx: &TyCtxt) -> Cost;
}
