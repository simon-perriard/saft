use core::fmt;
use rustc_middle::ty::TyCtxt;
use rustc_span::Span;
use std::collections::{HashMap, HashSet};

use super::cost_domain::FreshIdProvider;

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub(crate) enum Cost {
    Infinity,
    Scalar(u64),
    Parameter(CostParameter),
    Add(Vec<Cost>),
    ScalarMul(u64, Box<Cost>),
    ParameterMul(CostParameter, Box<Cost>),
    Max(Vec<Cost>),
    BigO(Box<Cost>),
}

#[derive(Eq, Ord, PartialEq, PartialOrd, Clone, Debug, Hash)]
pub(crate) struct Variable {
    pub id: u32,
    pub span: Option<Span>,
}

impl Variable {
    pub fn new(fresh_var_id_provider: FreshIdProvider, span: Option<Span>) -> Cost {
        let current_fresh_var_id = *fresh_var_id_provider.borrow();

        let new_var = Variable {
            id: current_fresh_var_id,
            span,
        };

        let new_var = Cost::Parameter(CostParameter::LengthOf(new_var));

        *fresh_var_id_provider.borrow_mut() = current_fresh_var_id + 1;

        new_var
    }
}

#[derive(Eq, Ord, PartialEq, PartialOrd, Clone, Debug, Hash)]
pub(crate) enum CostParameter {
    //TODO: instead of String, have some Variable
    ValueOf(String),
    SizeOf(String),
    ReadsOf(String),
    WritesOf(String),
    SizeDepositedOf(String),
    StepsOf(String),
    LengthOf(Variable),
    //TODO: move Log to Cost
    Log(String),
}

impl CostParameter {
    pub(crate) fn symbolic_mul(self, rhs: Cost) -> Cost {
        if rhs.is_zero() {
            return Cost::Scalar(0);
        } else if let Cost::Scalar(1) = rhs {
            return Cost::Parameter(self);
        }

        Cost::ParameterMul(self, Box::new(rhs))
    }
}

impl fmt::Display for CostParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CostParameter::ValueOf(s) => write!(f, "VALUEOF({})", s),
            CostParameter::SizeOf(s) => write!(f, "SIZEOF({})", s),
            CostParameter::ReadsOf(s) => write!(f, "READSOF({})", s),
            CostParameter::WritesOf(s) => write!(f, "WRITESOF({})", s),
            CostParameter::SizeDepositedOf(s) => write!(f, "SIZEDEPOSITEDOF({})", s),
            CostParameter::StepsOf(s) => write!(f, "STEPSOF({})", s),
            CostParameter::LengthOf(s) => write!(f, "LENGTHOF({:?})", s),
            CostParameter::Log(s) => write!(f, "LOG({})", s),
        }
    }
}

impl Default for Cost {
    fn default() -> Self {
        Cost::Scalar(0)
    }
}

impl Cost {
    pub(crate) fn reduce_expr(&self) -> Self {
        //TODO: Extract common Max and reduce more Max
        self.clone()
    }

    pub(crate) fn add_one(&self) -> Self {
        (*self).clone() + Self::Scalar(1)
    }

    pub(crate) fn is_zero(&self) -> bool {
        match self {
            Self::Infinity => false,
            Self::Scalar(x) => *x == 0,
            Self::Parameter(_) => false,
            Self::Add(chain) => chain
                .iter()
                .map(|x| x.is_zero())
                .fold(false, |accum, x| accum & x),
            Self::ParameterMul(_, b) => b.is_zero(),
            Self::ScalarMul(a, b) => *a == 0 || b.is_zero(),
            Self::Max(chain) => chain
                .iter()
                .map(|x| x.is_zero())
                .fold(false, |accum, x| accum & x),
            Self::BigO(c) => c.is_zero(),
        }
    }

    pub(crate) fn is_infinity(&self) -> bool {
        match self {
            Self::Infinity => true,
            Self::Scalar(_) => false,
            Self::Parameter(_) => false,
            Self::Add(chain) => chain
                .iter()
                .map(|x| x.is_zero())
                .fold(false, |accum, x| accum | x),
            Self::ParameterMul(_, b) => b.is_infinity(),
            Self::ScalarMul(_, b) => b.is_infinity(),
            Self::Max(chain) => chain
                .iter()
                .map(|x| x.is_zero())
                .fold(false, |accum, x| accum | x),
            Self::BigO(c) => c.is_infinity(),
        }
    }

    pub(crate) fn mul(self, rhs: Self) -> Self {
        if self.is_infinity() || rhs.is_infinity() {
            return Cost::Infinity;
        } else if self.is_zero() || rhs.is_zero() {
            return Cost::default();
        }

        match self.clone() {
            Cost::Parameter(sym) => sym.symbolic_mul(rhs),
            Cost::Scalar(_) | Cost::ScalarMul(_, _) => self.concrete_mul(rhs),
            Cost::BigO(_) => match rhs.clone() {
                Cost::Scalar(_) | Cost::ScalarMul(_, _) => self.concrete_mul(rhs),
                Cost::Parameter(sym) => sym.symbolic_mul(self),
                _ => unimplemented!("BIGO({:?})", self),
            },
            _ => match rhs.clone() {
                Cost::Scalar(_) | Cost::ScalarMul(_, _) => rhs.concrete_mul(self),
                _ => unimplemented!("{:?} --- {:?}", self, rhs),
            },
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
            return Self::ScalarMul(x*y, a);
        } else if let Self::Scalar(x) = rhs && let Self::ScalarMul(y, a) = self {
            // Compact notation: ((y)*a)*(x) with x and y concrete -> (x*y)*a
            return Self::ScalarMul(x*y, a);
        } else if let Self::Scalar(x) = self {
            return Self::ScalarMul(x, Box::new(rhs));
        } else if let Self::Scalar(x) = rhs {
            return Self::ScalarMul(x, Box::new(self));
        }
        unreachable!();
    }

    pub(crate) fn max(&self, rhs: Self) -> Self {
        if self.is_infinity() || rhs.is_infinity() {
            return Self::Infinity;
        } else if self.is_zero() {
            return rhs.clone();
        } else if rhs.is_zero() {
            return self.clone();
        } else if let Self::Max(x) = self.clone() && let Self::Max(y) = rhs {
            let mut unique_elements = HashSet::new();

            for e in x.iter() {
                if e.is_zero() {
                    continue;
                } else if e.is_infinity() {
                    return Self::Infinity;
                }
                unique_elements.insert(e.clone());
            }

            for e in y.iter() {
                if e.is_zero() {
                    continue;
                } else if e.is_infinity() {
                    return Self::Infinity;
                }
                unique_elements.insert(e.clone());
            }

            return Self::Max(unique_elements.drain().collect::<Vec<_>>());

        } else if let Self::Max(x) = self.clone() {
            if x.contains(&rhs) {
                return self.clone();
            } else {
                let mut max = x;
                max.push(rhs);
                return Self::Max(max);
            }
        } else if let Self::Max(y) = rhs.clone() {
            if y.contains(&rhs) {
                return (*self).clone();
            } else {
                let mut max = y;
                max.push(self.clone());
                return Self::Max(max);
            }
        } else {

            let add_self = Cost::Add(vec![self.clone()]);
            let add_rhs = Cost::Add(vec![rhs.clone()]);

            // Let's try to already apply some simple optimizations to the max
            if let Some(greatest) = add_self.cmp_add_chain(&add_rhs) {
                if let Cost::Add(chain) = greatest.clone() {
                    if chain.len() == 1 {
                        return chain[0].clone();
                    } else {
                        return greatest;
                    }
                } else {
                    unreachable!();
                }
            } else {
                return Self::Max(vec![(*self).clone(), rhs]);
            }
        }
    }

    fn flatten_add_chain(&self) -> Vec<Self> {
        let mut flat = Vec::new();

        match self {
            Self::Add(chain) => {
                for e in chain.iter() {
                    flat.append(&mut e.flatten_add_chain());
                }
            }
            Self::ScalarMul(a, b) => {
                // Open the concrete mul
                let mut flat_b = b.flatten_add_chain();
                for _ in 0..*a {
                    flat.append(&mut flat_b);
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
        matches!(self, Self::Add(_))
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
        } else if let Self::Add(x) = self.clone() && let Self::Scalar(s) = rhs {
            let mut vec = x.clone();
            let scalar = vec.drain_filter(|c| matches!(c, Self::Scalar(_))).map(|x| if let Self::Scalar(x) = x {x} else {unreachable!()}).fold(0, |accum, x| accum+x);

            vec.push(Cost::Scalar(scalar+s));
            if vec.len() == 1 {
                vec[0].clone()
            } else {
                Cost::Add(vec)
            }
        } else if let Self::Scalar(s) = self.clone() && let Self::Add(x) = rhs {
            let mut vec = x.clone();
            let scalar = vec.drain_filter(|c| matches!(c, Self::Scalar(_))).map(|x| if let Self::Scalar(x) = x {x} else {unreachable!()}).fold(0, |accum, x| accum+x);

            vec.push(Cost::Scalar(scalar+s));
            if vec.len() == 1 {
                vec[0].clone()
            } else {
                Cost::Add(vec)
            }
        } else {

            let wrapped = Cost::Add(vec![self, rhs]);
            let wrapped = wrapped.flatten_add_chain();

            let mut count_common = HashMap::new();

            for e in wrapped.iter() {
                if count_common.contains_key(&e) {
                    let current_count: &Cost = count_common.get(&e).unwrap();
                    count_common.insert(e, (*current_count).clone() + Cost::Scalar(1));
                } else {
                    count_common.insert(e, Cost::Scalar(1));
                }
            }

            let mut res = Vec::new();

            for (k, v) in count_common.iter() {
                res.push(((*k).clone()).concrete_mul(v.clone()).clone());
            }

            if res.len() == 1 {
                res[0].clone()
            } else {
                Cost::Add(res)
            }
        }
    }
}

impl fmt::Display for Cost {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Infinity => write!(f, "∞"),
            Self::Scalar(x) => write!(f, "{}", x),
            Self::Parameter(symbolic) => write!(f, "{}", symbolic),
            Self::Add(chain) => write!(
                f,
                "{}",
                chain
                    .iter()
                    .map(|c| format!("{}", c))
                    .reduce(|accum, c| format!("{}\n + {}\n", accum, c))
                    .unwrap_or_default()
            ),
            Self::ParameterMul(a, b) => {
                if b.pretty_print_need_parenthesis() {
                    write!(f, "{} * ({})", a, b)
                } else {
                    write!(f, "{} * {}", a, b)
                }
            }
            Self::ScalarMul(a, b) => write!(f, "{} * ({})", a, b),
            Self::Max(chain) => write!(
                f,
                "MAX(\n{}\n)",
                chain
                    .iter()
                    .map(|c| format!("{}", c))
                    .reduce(|accum, c| format!("{}\n ,\n{}\n", accum, c))
                    .unwrap_or_default()
            ),
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
        Cost::Parameter(s) => Cost::BigO(Box::new(Cost::Parameter(s))),
        Cost::Add(chain) => chain
            .iter()
            .map(|x| cost_to_big_o(x.clone()))
            .reduce(|accum, x| accum + x)
            .unwrap(),
        // Storage sizes are constructed such that lhs of mul is the number of elements, rhs is their size
        Cost::ScalarMul(a, _) => Cost::Scalar(a),
        Cost::ParameterMul(a, _) => cost_to_big_o(Cost::Parameter(a)),
        Cost::Max(chain) => chain
            .iter()
            .map(|x| cost_to_big_o(x.clone()))
            .reduce(|accum, x| accum.max(x))
            .unwrap(),
        Cost::BigO(_) => size,
    }
}

pub(crate) trait HasSize {
    fn get_size(&self, tcx: TyCtxt) -> Cost;
}

#[cfg(test)]
mod tests {
    use crate::analysis::cost_language::CostParameter;

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
    fn concrete_and_symbolic_add_chain_reduction() {
        let a = Cost::Scalar(1);
        let b = Cost::Parameter(CostParameter::SizeOf("sym".to_string()));
        let c = Cost::Scalar(1);

        assert_eq!(a + b.clone() + c, Cost::Add(vec![b, Cost::Scalar(2)]));
    }
}
