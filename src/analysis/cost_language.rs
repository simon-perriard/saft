use core::fmt;
use rustc_middle::ty::TyCtxt;
use std::collections::{HashMap, HashSet};


#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub(crate) enum Cost {
    Infinity,
    Scalar(u64),
    Variable(Symbolic),
    Add(Box<Cost>, Box<Cost>),
    ScalarMul(u64, Box<Cost>),
    VariableMul(Symbolic, Box<Cost>),
    Max(Box<Cost>, Box<Cost>),
    BigO(Box<Cost>),
}

#[derive(Eq, Ord, PartialEq, PartialOrd, Clone, Debug, Hash)]
pub(crate) enum Symbolic {
    ValueOf(String),
    SizeOf(String),
    ReadsOf(String),
    WritesOf(String),
    EventsOf(String),
    StepsOf(String),
    LengthOf(String),
    Log(String),
}

impl Symbolic {
    pub(crate) fn symbolic_mul(self, rhs: Cost) -> Cost {
        if rhs.is_zero() {
            return Cost::Scalar(0);
        } else if let Cost::Scalar(1) = rhs {
            return Cost::Variable(self);
        }

        Cost::VariableMul(self, Box::new(rhs))
    }
}

impl fmt::Display for Symbolic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbolic::ValueOf(s) => write!(f, "VALUEOF({})", s),
            Symbolic::SizeOf(s) => write!(f, "SIZEOF({})", s),
            Symbolic::ReadsOf(s) => write!(f, "READSOF({})", s),
            Symbolic::WritesOf(s) => write!(f, "WRITESOF({})", s),
            Symbolic::EventsOf(s) => write!(f, "EVENTSOF({})", s),
            Symbolic::StepsOf(s) => write!(f, "STEPSOF({})", s),
            Symbolic::LengthOf(s) => write!(f, "LENGTHOF({})", s),
            Symbolic::Log(s) => write!(f, "LOG({})", s),
        }
    }
}

impl Default for Cost {
    fn default() -> Self {
        Cost::Scalar(0)
    }
}

impl Cost {
    pub(crate) fn is_zero(&self) -> bool {
        match self {
            Self::Infinity => false,
            Self::Scalar(x) => *x == 0,
            Self::Variable(_) => false,
            Self::Add(a, b) => a.is_zero() && b.is_zero(),
            Self::VariableMul(_, b) => b.is_zero(),
            Self::ScalarMul(a, b) => *a == 0 || b.is_zero(),
            Self::Max(a, b) => a.is_zero() && b.is_zero(),
            Self::BigO(c) => c.is_zero(),
        }
    }

    pub(crate) fn is_infinity(&self) -> bool {
        match self {
            Self::Infinity => true,
            Self::Scalar(_) => false,
            Self::Variable(_) => false,
            Self::Add(a, b) => a.is_infinity() || b.is_infinity(),
            Self::VariableMul(_, b) => b.is_infinity(),
            Self::ScalarMul(a, b) => b.is_infinity(),
            Self::Max(a, b) => a.is_infinity() || b.is_infinity(),
            Self::BigO(c) => c.is_infinity(),
        }
    }

    pub(crate) fn mul(self, rhs: Self) -> Self {
        match self.clone() {
            Cost::Variable(sym) => sym.symbolic_mul(rhs),
            Cost::Scalar(_) | Cost::ScalarMul(_, _) => self.concrete_mul(rhs),
            Cost::BigO(_) => match rhs.clone() {
                Cost::Scalar(_) | Cost::ScalarMul(_, _) => self.concrete_mul(rhs),
                Cost::Variable(sym) => sym.symbolic_mul(self),
                _ => unimplemented!("BIGO({:?})", self)
            }
            _ => unimplemented!("{:?}", self),
        }
    }

    pub(crate) fn concrete_mul(self, rhs: Self) -> Self {
        if self.is_infinity() || rhs.is_infinity() {
            return Self::Infinity;
        } else if self.is_zero() || rhs.is_zero() {
            // Compact notation: x*y with x=0 and/or y=0 -> 0 
            return Self::Scalar(0);
        } else if let Self::Scalar(1) = self {
            // Compact notation: x*y with x=1 -> y
            return rhs;
        } else if let Self::Scalar(1) = rhs {
            // Compact notation: x*y with y=1 -> x
            return self;
        } else if let Self::Scalar(x) = self && let Self::Scalar(y) = rhs {
            // Compact notation: (x)*(y) with x and y concrete -> (x*y)
            return Self::Scalar(x*y);
        } else if let Self::Scalar(x) = self && let Self::ScalarMul(y, a) = rhs {
            // Compact notation: (x)*((y)*a) with x and y concrete -> (x*y)*a
            return Self::ScalarMul(x*y, Box::new(a.reduce_add_chain()));
        } else if let Self::Scalar(x) = rhs && let Self::ScalarMul(y, a) = self {
            // Compact notation: ((y)*a)*(x) with x and y concrete -> (x*y)*a
            return Self::ScalarMul(x*y, Box::new(a.reduce_add_chain()));
        } else if let Self::Scalar(x) = self {
            return Self::ScalarMul(x, Box::new(rhs.reduce_add_chain()));
        } else if let Self::Scalar(x) = rhs {
            return Self::ScalarMul(x, Box::new(self.reduce_add_chain()));
        }
        unreachable!();
    }

    pub(crate) fn max(&self, rhs: Self) -> Self {
        if self.is_infinity() || rhs.is_infinity() {
            return Self::Infinity;
        }else if self.is_zero() {
            return rhs.reduce_add_chain();
        } else if rhs.is_zero() || *self == rhs {
            return self.reduce_add_chain();
        } else if let Self::Scalar(x) = *self && let Self::Scalar(y) = rhs {
            // Compact notation: max(x, y) => x > y ? x : y
            return if x > y {self.reduce_add_chain()} else {rhs.reduce_add_chain()};
        } else if let Self::Max(a, _) = rhs.clone() && *self == *a {
            // Compact notation: max(a, max(a, b)) => max(a, b)
            return rhs.reduce_add_chain();
        }  else if let Self::Max(a, _) = (*self).clone() && rhs == *a {
            // Compact notation: max(max(a,b), a) => max(a, b)
            return self.reduce_add_chain();
        } else if let Self::Max(_, b) = rhs.clone() && *self == *b {
            // Compact notation: max(b, max(a, b)) => max(a, b)
            return rhs.reduce_add_chain();
        } else if let Self::Max(_, b) = (*self).clone() && rhs == *b {
            // Compact notation: max(max(a, b), b) => max(a, b)
            return self.reduce_add_chain();
        }

        // Try to resolve the max
        if let Some(longest) = self.cmp_add_chain(&rhs.reduce_add_chain()) {
            return longest;
        }

        Self::Max(
            Box::new(self.reduce_add_chain()),
            Box::new(rhs.reduce_add_chain()),
        )
        .extract_max_add_common()
    }

    pub(crate) fn max_reduced(&self, rhs: Self) -> Self {
        if self.is_zero() {
            return rhs;
        } else if rhs.is_zero() || *self == rhs {
            return self.clone();
        } else if let Self::Scalar(x) = *self && let Self::Scalar(y) = rhs {
            // Compact notation: max(x, y) => x > y ? x : y
            return if x > y {self.clone()} else {rhs};
        } else if let Self::Max(a, _) = rhs.clone() && *self == *a {
            // Compact notation: max(a, max(a, b)) => max(a, b)
            return rhs;
        }  else if let Self::Max(a, _) = (*self).clone() && rhs == *a {
            // Compact notation: max(max(a,b), a) => max(a, b)
            return self.clone();
        } else if let Self::Max(_, b) = rhs.clone() && *self == *b {
            // Compact notation: max(b, max(a, b)) => max(a, b)
            return rhs;
        } else if let Self::Max(_, b) = (*self).clone() && rhs == *b {
            // Compact notation: max(max(a, b), b) => max(a, b)
            return self.clone();
        }

        // Try to resolve the max
        if let Some(longest) = self.cmp_add_chain(&rhs) {
            return longest;
        }

        Self::Max(Box::new(self.clone()), Box::new(rhs)).extract_max_add_common()
    }

    fn flatten_add_chain(&self) -> Vec<Self> {
        let mut flat = Vec::new();

        match self {
            Self::Add(a, b) => {
                flat.append(&mut a.flatten_add_chain());
                flat.append(&mut b.flatten_add_chain());
            }
            Self::ScalarMul(a, b) => {
                // Open the concrete mul
                for _ in 0..*a {
                    flat.append(&mut b.flatten_add_chain());
                }
            }
            _ => flat.push(self.clone()),
        }

        flat
    }

    // Compare an Add or Self with another Add
    fn cmp_add_chain(&self, other: &Self) -> Option<Self> {
        let mut chain_1 = self.flatten_add_chain();
        let mut chain_2 = other.flatten_add_chain();

        // Extract concretes and compute the diff
        let aggregated_concretes_1 = chain_1
            .drain_filter(|c| matches!(c, Cost::Scalar(_)))
            .reduce(|accum, item| accum + item)
            .unwrap_or_default();

        let aggregated_concretes_2 = chain_2
            .drain_filter(|c| matches!(c, Cost::Scalar(_)))
            .reduce(|accum, item| accum + item)
            .unwrap_or_default();

        if let Cost::Scalar(concrete_1) = aggregated_concretes_1 && let Cost::Scalar(concrete_2) = aggregated_concretes_2 {
            match concrete_1.cmp(&concrete_2) {
                std::cmp::Ordering::Less => chain_2.push(Cost::Scalar(concrete_2-concrete_1)),
                std::cmp::Ordering::Equal => (),
                std::cmp::Ordering::Greater => chain_1.push(Cost::Scalar(concrete_1-concrete_2)),
            }
        } else {
            unreachable!();
        }

        // Remove common elements from chain_1 and chain_2
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

    pub fn reduce_add_chain(&self) -> Self {
        //return self.clone();
        let res;
        match self {
            Cost::Add(_, _) => {
                let mut flat = self.flatten_add_chain();

                // Extract and aggregate concretes
                let aggregated_concretes = flat
                    .drain_filter(|c| matches!(c, Cost::Scalar(_)))
                    .reduce(|accum, item| accum + item)
                    .unwrap_or_default();

                // Extract and reduce the BigO elements
                let big_os = flat
                    .drain_filter(|c| matches!(c, Cost::BigO(_)))
                    .fold(HashSet::new(), |mut acc, item| {
                        acc.insert(item);
                        acc
                    })
                    .drain()
                    .reduce(|accum, item| Cost::Add(Box::new(accum), Box::new(item)))
                    .unwrap_or_default();

                // Extract reduce the other symbolic elements
                let other_symbolics = flat
                    .drain_filter(|c| matches!(c, Cost::Variable(_)))
                    .fold(HashMap::new(), |mut acc, item| {
                        if acc.contains_key(&item) {
                            let current_count: &Cost = acc.get(&item).unwrap();
                            acc.insert(item, (*current_count).clone() + Cost::Scalar(1));
                        } else {
                            acc.insert(item, Cost::Scalar(1));
                        }
                        acc
                    })
                    .drain()
                    .fold(Cost::default(), |accum, (k, v)| {
                        if accum.is_zero() {
                            k.concrete_mul(v)
                        } else {
                            Cost::Add(Box::new(accum), Box::new(k.concrete_mul(v)))
                        }
                    });

                // Push back the aggregated symbols
                flat.push(other_symbolics);

                // Push back the aggregated concretes
                flat.push(aggregated_concretes);

                // Push back the reduced BigO formulae
                flat.push(big_os);

                // drain instead of iter because we need the object, not the reference
                res = flat
                    .drain_filter(|item| !item.is_zero())
                    .reduce(|accum, item| Cost::Add(Box::new(accum), Box::new(item)))
                    .unwrap_or_default();
            }
            Cost::ScalarMul(a, b) => res = Cost::ScalarMul(*a, Box::new(b.reduce_add_chain())),
            Cost::VariableMul(a, b) => {
                res = Cost::VariableMul((*a).clone(), Box::new(b.reduce_add_chain()))
            }
            Cost::Max(a, b) => res = a.reduce_add_chain().max_reduced(b.reduce_add_chain()),
            _ => res = (*self).clone(),
        }
        res
    }

    fn extract_max_add_common(&self) -> Self {
        //return self.clone();
        match self {
            Cost::Max(a, b) => {
                let mut chain_1 = a.reduce_add_chain().flatten_add_chain();
                let mut chain_2 = b.reduce_add_chain().flatten_add_chain();

                let extracted_common_add_chain = chain_1
                    .drain_filter(|cost_1| {
                        let res = chain_2.contains(cost_1);
                        if res {
                            let chain_2_index = chain_2
                                .iter()
                                .position(|cost_2| *cost_1 == *cost_2)
                                .unwrap();
                            chain_2.remove(chain_2_index);
                        }
                        res
                    })
                    .reduce(|accum, item| accum + item)
                    .unwrap_or_default();

                // drain instead of iter because we need the object, not the reference
                let lhs = Box::new(
                    chain_1
                        .drain(0..)
                        .reduce(|accum, item| accum + item)
                        .unwrap_or_default(),
                );
                let rhs = Box::new(
                    chain_2
                        .drain(0..)
                        .reduce(|accum, item| accum + item)
                        .unwrap_or_default(),
                );

                Cost::Add(
                    Box::new(extracted_common_add_chain),
                    Box::new(Cost::Max(lhs, rhs)),
                )
            }
            _ => unreachable!("This must only be applied on Cost::Max"),
        }
    }
}

impl std::ops::Add for Cost {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        if self.is_infinity() || rhs.is_infinity() {
            return Self::Infinity;
        } else if self.is_zero() {
            return rhs;
        } else if rhs.is_zero() {
            return self;
        } else if let Self::Scalar(x) = self && let Self::Scalar(y) = rhs {
            return Self::Scalar(x+y);
        } else if let Self::ScalarMul(x, a) = self.clone() && *a == rhs {
            return Self::ScalarMul(x+1, a);
        } else if let Self::ScalarMul(x, a) = rhs.clone() && *a == self {
            return Self::ScalarMul(x+1, a);
        } else if let Self::ScalarMul(x, a) = self.clone() && let Self::ScalarMul(y, b) = rhs.clone() && *a == *b{
            return Self::ScalarMul(x+y, a);
        } else if self == rhs {
            return Self::ScalarMul(2, Box::new(self));
        }

        Self::Add(Box::new(self), Box::new(rhs))
    }
}

impl fmt::Display for Cost {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Infinity => write!(f, "∞"),
            Self::Scalar(x) => write!(f, "{}", x),
            Self::Variable(symbolic) => write!(f, "{}", symbolic),
            Self::Add(a, b) => write!(f, "{}\n + \n{}", a, b),
            Self::VariableMul(a, b) => {
                if b.pretty_print_need_parenthesis() {
                    write!(f, "{} * ({})", a, b)
                } else {
                    write!(f, "{} * {}", a, b)
                }
            }
            Self::ScalarMul(a, b) => write!(f, "{} * ({})", a, b),
            Self::Max(a, b) => write!(f, "MAX(\n\t{}\n , \n\t{}\n)", a, b),
            Self::BigO(s) => write!(f, "O({})", s),
        }
    }
}

pub(crate) fn cost_to_big_o(size: Cost) -> Cost {
    if size.is_zero() {
        return Cost::Scalar(0);
    }

    // TODO: ensure this is correct
    match size.clone() {
        Cost::Infinity => Cost::Infinity,
        Cost::Scalar(_) => Cost::Scalar(1),
        Cost::Variable(s) => Cost::BigO(Box::new(Cost::Variable(s))),
        Cost::Add(a, b) => cost_to_big_o(*a) + cost_to_big_o(*b),
        // Storage sizes are constructed such that lhs of mul is the number of elements, rhs is their size
        Cost::ScalarMul(a, _) => Cost::Scalar(a),
        Cost::VariableMul(a, _) => cost_to_big_o(Cost::Variable(a)),
        Cost::Max(a, b) => cost_to_big_o(*a).max(cost_to_big_o(*b)),
        Cost::BigO(_) => size,
    }
}

pub(crate) trait HasSize {
    fn get_size(&self, tcx: TyCtxt) -> Cost;
}

#[cfg(test)]
mod tests {
    use crate::analysis::cost_language::Symbolic;

    use super::Cost;

    #[test]
    fn concrete_add_chain_reduction() {
        let a = Cost::Scalar(1);
        let b = Cost::Scalar(1);

        assert_eq!(a + b, Cost::Scalar(2))
    }

    #[test]
    fn concrete_max_add_chain_reduction() {
        let a = Cost::Scalar(1);
        let b = Cost::Scalar(1);
        let c = Cost::Scalar(2);
        let d = Cost::Scalar(2);

        let max = (a + b).max(c + d);

        assert_eq!(max, Cost::Scalar(4));
    }

    #[test]
    fn mixed_add_chain_reduction() {
        let a = Cost::Scalar(1);
        let b = Cost::Scalar(1);
        let s = Cost::Variable(Symbolic::ValueOf(format!("sym")));

        let chain = a + s.clone() + b;

        assert_eq!(chain.reduce_add_chain(), s + Cost::Scalar(2));
    }

    #[test]
    fn mixed_max_add_chain() {
        let a = Cost::Scalar(1);
        let b = Cost::Scalar(1);
        let c = Cost::Scalar(2);
        let d = Cost::Scalar(2);
        let s = Cost::Variable(Symbolic::ValueOf(format!("sym")));

        let chain1 = a + s.clone() + b;
        let chain2 = c + s.clone() + d;

        assert_eq!(
            Cost::Max(Box::new(chain1), Box::new(chain2)).reduce_add_chain(),
            s + Cost::Scalar(4)
        );
    }

    #[test]
    fn extract_max_add_common() {
        let s1 = Cost::Variable(Symbolic::ValueOf(format!("sym1")));
        let s2 = Cost::Variable(Symbolic::ValueOf(format!("sym2")));
        let s3 = Cost::Variable(Symbolic::ValueOf(format!("sym3")));

        let chain1 = s1.clone() + s2.clone();
        let chain2 = s2.clone() + s3.clone();

        assert_eq!(
            Cost::Max(Box::new(chain1), Box::new(chain2)).extract_max_add_common(),
            Cost::Add(
                Box::new(s2),
                Box::new(Cost::Max(Box::new(s1), Box::new(s3)))
            )
        )
    }

    #[test]
    fn max_opens_concrete_mul() {
        let s1 = Cost::Variable(Symbolic::ValueOf(format!("sym1")));

        let c_mul = s1.clone() + s1.clone();

        assert_eq!(c_mul, Cost::ScalarMul(2, Box::new(s1.clone())));

        let max = s1.clone().max(c_mul.clone());

        assert_eq!(max, c_mul)
    }
}
