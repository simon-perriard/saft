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
}

impl Size {
    pub(crate) fn concrete(c: u128) -> Self {
        Size::UnitSize(Box::new(UnitSize::Concrete(c)))
    }

    pub(crate) fn symbolic(s: String) -> Self {
        Size::UnitSize(Box::new(UnitSize::Symbolic(s)))
    }

    pub(crate) fn interval(a: Size, b: Size) -> Self {
        Size::UnitSize(Box::new(UnitSize::Interval(a, b)))
    }

    pub(crate) fn unit() -> Self {
        Size::UnitSize(Box::new(UnitSize::Unit))
    }

    pub(crate) fn is_zero(&self) -> bool {
        match self {
            Size::UnitSize(unit_size) => unit_size.is_zero(),
            Size::Operation(op) => op.is_zero(),
        }
    }
}

impl UnitSize {
    pub(crate) fn is_zero(&self) -> bool {
        match self {
            UnitSize::Concrete(x) => *x == 0,
            UnitSize::Symbolic(_) => false,
            UnitSize::Interval(a, b) => a.is_zero() && b.is_zero(),
            UnitSize::Unit => true,
        }
    }
}

impl Operation {
    pub(crate) fn is_zero(&self) -> bool {
        match self {
            Operation::Add(a, b) => a.is_zero() && b.is_zero(),
            Operation::Mul(a, b) => a.is_zero() || b.is_zero(),
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
            UnitSize::Concrete(x) => write!(f, "{}", x),
            UnitSize::Symbolic(s) => write!(f, "SIZEOF({})", s),
            UnitSize::Interval(a, b) => write!(f, "[{},{}]", a, b),
            UnitSize::Unit => write!(f, "0"),
        }
    }
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operation::Add(a, b) => write!(f, "{} + {}", a, b),
            Operation::Mul(a, b) => write!(f, "({}) * ({})", a, b),
        }
    }
}

impl fmt::Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Size::UnitSize(unit) => write!(f, "{}", unit),
            Size::Operation(op) => write!(f, "{}", op),
        }
    }
}

impl std::ops::Add for Size {
    type Output = Size;

    fn add(self, rhs: Self) -> Self::Output {
        if self.is_zero() && rhs.is_zero() {
            Size::unit()
        } else if self.is_zero() {
            rhs
        } else if rhs.is_zero() {
            self
        } else {
            Size::Operation(Box::new(Operation::Add(self, rhs)))
        }
    }
}

impl std::ops::Mul for Size {
    type Output = Size;

    fn mul(self, rhs: Self) -> Self::Output {
        if self.is_zero() {
            rhs
        } else if rhs.is_zero() {
            self
        } else {
            Size::Operation(Box::new(Operation::Mul(self, rhs)))
        }
    }
}

pub(crate) trait HasSize {
    fn get_size(&self, tcx: &TyCtxt) -> Size;
}
