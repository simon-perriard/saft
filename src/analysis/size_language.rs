use core::fmt;

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
}

impl Default for Size {
    fn default() -> Self {
        Size::UnitSize(Box::new(UnitSize::Concrete(0)))
    }
}

impl fmt::Display for UnitSize {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnitSize::Concrete(x) => write!(f, "{}", x),
            UnitSize::Symbolic(s) => write!(f, "{}", s),
            UnitSize::Interval(a, b) => write!(f, "[{},{}]", a, b),
            UnitSize::Unit => write!(f, "()"),
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
        Size::Operation(Box::new(Operation::Add(self, rhs)))
    }
}

impl std::ops::Mul for Size {
    type Output = Size;

    fn mul(self, rhs: Self) -> Self::Output {
        Size::Operation(Box::new(Operation::Mul(self, rhs)))
    }
}
