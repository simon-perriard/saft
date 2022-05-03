use core::fmt;
use rustc_middle::ty::TyCtxt;

#[derive(Eq, PartialEq, Clone, Debug)]
pub(crate) enum UnitSize {
    Concrete(u128),
    Symbolic(String),
    Interval(Size, Size),
    Unit,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub(crate) enum Size {
    UnitSize(Box<UnitSize>),
    Operation(Box<Operation>),
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub(crate) enum Operation {
    Add(Size, Size),
    Mul(Size, Size),
    Max(Size, Size),
}

impl Size {
    pub(crate) fn concrete(c: u128) -> Self {
        Self::UnitSize(Box::new(UnitSize::Concrete(c)))
    }

    pub(crate) fn symbolic(s: String) -> Self {
        Self::UnitSize(Box::new(UnitSize::Symbolic(s)))
    }

    pub(crate) fn interval(a: Size, b: Size) -> Self {
        Self::UnitSize(Box::new(UnitSize::Interval(a, b)))
    }

    pub(crate) fn unit() -> Self {
        Self::UnitSize(Box::new(UnitSize::Unit))
    }

    pub(crate) fn is_zero(&self) -> bool {
        match self {
            Self::UnitSize(unit_size) => unit_size.is_zero(),
            Self::Operation(op) => op.is_zero(),
        }
    }

    pub(crate) fn max(&self, other: &Self) -> Self {
        if *self == *other {
            (*self).clone()
        } else if self.is_zero() {
            (*other).clone()
        } else if other.is_zero() {
            (*self).clone()
        } else {
            Self::Operation(Box::new(Operation::Max((*self).clone(), (*other).clone())))
        }
    }
}

impl UnitSize {
    pub(crate) fn is_zero(&self) -> bool {
        match self {
            Self::Concrete(x) => *x == 0,
            Self::Symbolic(_) => false,
            Self::Interval(a, b) => a.is_zero() && b.is_zero(),
            Self::Unit => true,
        }
    }
}

impl Operation {
    pub(crate) fn is_zero(&self) -> bool {
        match self {
            Self::Add(a, b) => a.is_zero() && b.is_zero(),
            Self::Mul(a, b) => a.is_zero() || b.is_zero(),
            Self::Max(a, b) => a.is_zero() && b.is_zero(),
        }
    }
}

impl Default for Size {
    fn default() -> Self {
        Self::unit()
    }
}

impl fmt::Display for UnitSize {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Concrete(x) => write!(f, "{}", x),
            Self::Symbolic(s) => write!(f, "SIZEOF({})", s),
            Self::Interval(a, b) => write!(f, "[{},{}]", a, b),
            Self::Unit => write!(f, "0"),
        }
    }
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add(a, b) => write!(f, "{} + {}", a, b),
            Self::Mul(a, b) => {
                if let Size::Operation(_) = a && let Size::Operation(_) = b {
                    write!(f, "({}) * ({})", a, b)
                } else if let Size::Operation(_) = a {
                    write!(f, "({}) * {}", a, b)
                } else if let Size::Operation(_) = b {
                    write!(f, "{} * ({})", a, b)
                } else {
                    write!(f, "{} * {}", a, b)
                }
            }
            Self::Max(a, b) => write!(f, "MAX({}, {})", a, b),
        }
    }
}

impl fmt::Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnitSize(unit) => write!(f, "{}", unit),
            Self::Operation(op) => write!(f, "{}", op),
        }
    }
}

impl std::ops::Add for Size {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self.is_zero() && rhs.is_zero() {
            Self::unit()
        } else if self.is_zero() {
            rhs
        } else if rhs.is_zero() {
            self
        } else {
            if let Self::UnitSize(box UnitSize::Concrete(x)) = self &&
            let Self::UnitSize(box UnitSize::Concrete(y)) = rhs {
                return Self::concrete(x+y);
            }
            else if self == rhs {
                // Compact notation: a+a = 2*a
                return Self::concrete(2) * self;
            } else if let Self::Operation(box Operation::Mul(a, b)) = self.clone() && (a == rhs || b == rhs)
            {
                // Compact notation: self+rhs = a*b + a or a*b + b => (a+1)*b or a*(b+1)
                if let Self::UnitSize(box UnitSize::Concrete(x)) = a {
                    return Self::concrete(x+1) * b;
                } else if let Self::UnitSize(box UnitSize::Concrete(x)) = b {
                    return Self::concrete(x+1) * a;
                }
            } else if let Self::Operation(box Operation::Mul(a, b)) = rhs.clone() && (a == self || b == self) {
                // Compact notation: self+rhs = a + a*b or b + a*b => (a+1)*b or a*(b+1)
                if let Self::UnitSize(box UnitSize::Concrete(x)) = a {
                    return Self::concrete(x+1) * b;
                } else if let Self::UnitSize(box UnitSize::Concrete(x)) = b {
                    return Self::concrete(x+1) * a;
                }
            } else if let Self::Operation(box Operation::Mul(Size::UnitSize(box UnitSize::Concrete(x)), a)) = self.clone() &&
                let Self::Operation(box Operation::Mul(Size::UnitSize(box UnitSize::Concrete(y)), b)) = rhs.clone() &&
                a == b {
                    // Compact notation: self+rhs = x*a + y*a = (x+y)*a
                    return Self::concrete(x+y) * a;
                }

            Self::Operation(Box::new(Operation::Add(self, rhs)))
        }
    }
}

impl std::ops::Mul for Size {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        if self.is_zero() {
            rhs
        } else if rhs.is_zero() {
            self
        } else {
            if let Self::UnitSize(box UnitSize::Concrete(x)) = self &&
                let Self::Operation(box Operation::Mul(a, b)) = rhs.clone() &&
                let Self::UnitSize(box UnitSize::Concrete(y)) = a
            {
                return Self::concrete(x*y) * b
            }

            Self::Operation(Box::new(Operation::Mul(self, rhs)))
        }
    }
}

pub(crate) trait HasSize {
    fn get_size(&self, tcx: &TyCtxt) -> Size;
}
