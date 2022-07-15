use core::fmt;
use rustc_middle::ty::TyCtxt;
use rustc_span::Span;
use std::collections::{HashMap, HashSet};

use super::cost_domain::FreshIdProvider;

#[derive(Eq, Clone, Debug, Hash, PartialOrd)]
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

impl PartialEq for Cost {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Scalar(l0), Self::Scalar(r0)) => l0 == r0,
            (Self::Parameter(l0), Self::Parameter(r0)) => l0 == r0,
            (Self::Add(l0), Self::Add(r0)) => {
                let mut l0_mut = l0.clone();
                let mut r0_mut = r0.clone();

                l0_mut.drain_filter(|c| {
                    if r0_mut.contains(c) {
                        let mut idx_to_remove = 0;

                        for (idx, e) in r0_mut.iter().enumerate() {
                            if e == c {
                                idx_to_remove = idx;
                                break;
                            }
                        }

                        r0_mut.remove(idx_to_remove);

                        true
                    } else {
                        false
                    }
                });

                l0_mut.is_empty() && r0_mut.is_empty()
            }
            (Self::ScalarMul(l0, l1), Self::ScalarMul(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::ParameterMul(l0, l1), Self::ParameterMul(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Max(l0), Self::Max(r0)) => {
                let mut l0_mut = l0.clone();
                let mut r0_mut = r0.clone();

                l0_mut.drain_filter(|c| {
                    if r0_mut.contains(c) {
                        let mut idx_to_remove = 0;

                        for (idx, e) in r0_mut.iter().enumerate() {
                            if e == c {
                                idx_to_remove = idx;
                                break;
                            }
                        }

                        r0_mut.remove(idx_to_remove);

                        true
                    } else {
                        false
                    }
                });

                l0_mut.is_empty() && r0_mut.is_empty()
            }
            (Self::BigO(l0), Self::BigO(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

#[derive(Eq, Ord, PartialEq, Clone, Debug, Hash)]
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

impl PartialOrd for Variable {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.span.partial_cmp(&other.span) {
            Some(core::cmp::Ordering::Equal) => {
                // declared at the same span, typically a BoundedVec, which has a Vec inside,
                // we keep track of the more general one (BoundedVec), which is declared last (has higher id)
                self.id.partial_cmp(&other.id)
            }
            _ => {
                match self.id.partial_cmp(&other.id) {
                    // Lesser id was declared before, we keep the earliest declaration
                    Some(core::cmp::Ordering::Less) => Some(core::cmp::Ordering::Greater),
                    Some(core::cmp::Ordering::Equal) => Some(core::cmp::Ordering::Equal),
                    Some(core::cmp::Ordering::Greater) => Some(core::cmp::Ordering::Less),
                    None => None,
                }
            }
        }
    }
}

#[derive(Eq, Ord, PartialEq, PartialOrd, Clone, Debug, Hash)]
pub(crate) enum CostParameter {
    ValueOf(String),
    SizeOf(String),
    ReadsOf(String),
    WritesOf(String),
    SizeDepositedOf(String),
    StepsOf(String),
    LengthOf(Variable),
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
    fn flatten_max(&self) -> Vec<Self> {
        let mut flat = Vec::new();

        match self {
            Self::Max(chain) => {
                for e in chain.iter() {
                    let e_flat = e.flatten_max();

                    for c in e_flat.iter() {
                        if !flat.contains(c) {
                            flat.push(c.clone());
                        }
                    }
                }
            }
            _ => flat.push(self.clone()),
        }

        flat
    }

    fn recursive_search(&self, looking_for: Self) -> bool {
        match self {
            Cost::Infinity => *self == looking_for,
            Cost::Scalar(_) => *self == looking_for,
            Cost::Parameter(_) => *self == looking_for,
            Cost::Add(chain) => {
                if let Self::Add(_) = looking_for {
                    *self == looking_for
                } else {
                    chain
                        .iter()
                        .map(|c| c.recursive_search(looking_for.clone()))
                        .fold(false, |accum, r| accum | r)
                }
            }
            Cost::ScalarMul(_, _) => {
                if let Self::ScalarMul(_, _) = looking_for {
                    *self == looking_for
                } else {
                    false
                }
            }
            Cost::ParameterMul(_, _) => {
                if let Self::ParameterMul(_, _) = looking_for {
                    *self == looking_for
                } else {
                    false
                }
            }
            Cost::Max(chain) => {
                if let Self::Max(_) = looking_for {
                    *self == looking_for
                } else {
                    chain
                        .iter()
                        .map(|c| c.recursive_search(looking_for.clone()))
                        .fold(false, |accum, r| accum | r)
                }
            }
            Cost::BigO(c) => c.recursive_search(looking_for),
        }
    }

    pub(crate) fn reduce_expr(&self) -> Self {
        match self {
            Self::Infinity => Self::Infinity,
            Self::Scalar(_) => (*self).clone(),
            Self::Parameter(_) => (*self).clone(),
            Self::Add(chain) => chain
                .iter()
                .map(|expr| expr.reduce_expr())
                .reduce(|accum, expr| accum + expr)
                .unwrap(),
            Self::ScalarMul(a, b) => Self::ScalarMul(*a, Box::new(b.reduce_expr())),
            Self::ParameterMul(a, b) => Self::ParameterMul((*a).clone(), Box::new(b.reduce_expr())),
            Self::Max(_) => {
                // Extract common elements
                let mut reduced = self
                    .flatten_max()
                    .iter()
                    .map(|expr| expr.reduce_expr().flatten_add_chain())
                    .collect::<Vec<Vec<Cost>>>();
                let mut reduced_head = reduced.remove(0);
                let mut reduced_tail = reduced;

                let mut commons = reduced_head
                    .drain_filter(|expr| {
                        // For the current element in HEAD
                        let mut expr_idx = Vec::new();


                        for sub_vec in reduced_tail.iter() {
                            // Check whether we can find this element in each subvector of TAIL
                            for (idx, try_match_expr) in sub_vec.iter().enumerate() {
                                if expr == try_match_expr {
                                    expr_idx.push(idx);
                                    break;
                                }
                            }
                        }

                        if expr_idx.len() == reduced_tail.len() {
                            // We found the expr in every element
                            // Do a round to remove it in the tail sub vectors

                            for (sub_vec, idx_to_remove) in reduced_tail.iter_mut().zip(expr_idx) {
                                sub_vec.remove(idx_to_remove);
                            }

                            true
                        } else {
                            false
                        }
                    })
                    .collect::<Vec<Cost>>();

                if commons.is_empty() {
                    // Check whether elements are recursively present in the Max epxression
                    let mut vec_check_rec = reduced_tail.clone();
                    vec_check_rec.push(reduced_head);

                    let mut to_remove = Vec::new();

                    for (idx_outer, looking_for) in vec_check_rec.clone().iter().enumerate() {
                        for (idx_inner, try_find_in) in vec_check_rec.clone().iter().enumerate() {
                            if idx_outer == idx_inner {
                                continue;
                            } else if try_find_in
                                .iter()
                                .map(|c| {
                                    c.recursive_search(if looking_for.len() > 1 {
                                        Self::Add(looking_for.clone())
                                    } else {
                                        looking_for[0].clone()
                                    })
                                })
                                .fold(false, |accum, x| accum | x)
                            {
                                to_remove.push(idx_outer);
                            }
                        }
                    }

                    if to_remove.is_empty() {
                        return (*self).clone();
                    } else {
                        to_remove.reverse();

                        for idx_to_remove in to_remove.iter() {
                            vec_check_rec.remove(*idx_to_remove);
                        }

                        return vec_check_rec
                            .drain(0..)
                            .map(|mut max_monomial| {
                                max_monomial
                                    .drain(0..)
                                    .fold(Cost::default(), |accum, expr| accum + expr)
                            })
                            .fold(Cost::default(), |accum, max_monomial| {
                                accum.max(max_monomial)
                            })
                            .reduce_expr();
                    }
                }

                // Reconstruct the reduced expression
                // Rebuild the common add chain and add again the Maxs
                let common_add_chain = commons
                    .drain(0..)
                    .reduce(|accum, expr| accum + expr)
                    .unwrap();

                reduced_tail.push(reduced_head);

                let new_reduced_max = reduced_tail
                    .drain(0..)
                    .map(|mut max_monomial| {
                        max_monomial
                            .drain(0..)
                            .fold(Cost::default(), |accum, expr| accum + expr)
                    })
                    .fold(Cost::default(), |accum, max_monomial| {
                        accum.max(max_monomial)
                    });
                
                common_add_chain + new_reduced_max
            }
            Self::BigO(_) => (*self).clone(),
        }
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
            Self::Infinity
        } else if self.is_zero() {
            rhs.clone()
        } else if rhs.is_zero() {
            self.clone()
        } else if let Self::Max(x) = self.clone() && let Self::Max(y) = rhs.clone() {
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

            Self::Max(unique_elements.drain().collect::<Vec<_>>()).reduce_expr()

        } else if let Self::Max(x) = self.clone() {
            if x.contains(&rhs) {
                self.clone()
            } else {
                let mut max = x;
                // Remove elements that are subvec of rhs, and check whether rhs is a subvec of an element in the max
                let mut is_rhs_sub_vec = false;

                let wrapped_rhs = Cost::Add(vec![rhs.clone()]);

                let removed = max.drain_filter(|c| {
                    let wrapped_c = Cost::Add(vec![c.clone()]);

                    let cmp = wrapped_c.cmp_add_chain(&wrapped_rhs);

                    if let Some(r) = cmp.clone() && r == wrapped_c {
                        // self is a subset of wrapped_c
                        is_rhs_sub_vec = true;
                        false
                    } else if let Some(r) = cmp && r == wrapped_rhs {
                        true
                    } else {
                        false
                    }

                }).collect::<Vec<_>>();

                // If elements were removed, self is bigger and we push add it to the max
                if !removed.is_empty() {
                    if max.is_empty() {
                        self.clone().reduce_expr()
                    } else {
                        max.push(rhs);
                        Self::Max(max).reduce_expr()
                    }
                } else {
                    // No element were removed so either we were not able to compare, or self is smaller than an element
                    // In the first case we add it to the max
                    // In the second case we do not
                    if !is_rhs_sub_vec {
                        max.push(rhs);
                        Self::Max(max).reduce_expr()
                    } else {
                        self.clone().reduce_expr()
                    }
                }
            }
        } else if let Self::Max(y) = rhs.clone() {
            if y.contains(&rhs) {
                (*self).clone()
            } else {
                let mut max = y;
                // Remove elements that are subvec of self, and check whether self is a subvec of an element in the max
                let mut is_self_sub_vec = false;

                let wrapped_rhs = Cost::Add(vec![self.clone()]);

                let removed = max.drain_filter(|c| {
                    let wrapped_c = Cost::Add(vec![c.clone()]);

                    let cmp = wrapped_c.cmp_add_chain(&wrapped_rhs);

                    if let Some(r) = cmp.clone() && r == wrapped_c {
                        // self is a subset of wrapped_c
                        is_self_sub_vec = true;
                        false
                    } else if let Some(r) = cmp && r == wrapped_rhs {
                        true
                    } else {
                        false
                    }

                }).collect::<Vec<_>>();

                // If elements were removed, self is bigger and we push add it to the max
                if !removed.is_empty() {
                    if max.is_empty() {
                        self.clone().reduce_expr()
                    } else {
                        max.push(self.clone());
                        Self::Max(max).reduce_expr()
                    }
                } else {
                    // No element were removed so either we were not able to compare, or self is smaller than an element
                    // In the first case we add it to the max
                    // In the second case we do not
                    if !is_self_sub_vec {
                        max.push(self.clone());
                        Self::Max(max).reduce_expr()
                    } else {
                        self.clone().reduce_expr()
                    }
                }
            }
        } else {

            let add_self = Cost::Add(vec![self.reduce_expr()]);
            let add_rhs = Cost::Add(vec![rhs.reduce_expr()]);

            // Let's try to already apply some simple optimizations to the max
            if let Some(greatest) = add_self.cmp_add_chain(&add_rhs) {
                if let Cost::Add(chain) = greatest.clone() {
                    if chain.len() == 1 {
                        chain[0].reduce_expr()
                    } else {
                        greatest.reduce_expr()
                    }
                } else {
                    unreachable!();
                }
            } else {
                Self::Max(vec![self.clone(), rhs.clone()]).reduce_expr()
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
            Self::Scalar(x) => {
                for _ in 0..*x {
                    flat.push(Cost::Scalar(1));
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
            Self::Infinity
        } else if self.is_zero() {
            rhs
        } else if rhs.is_zero() {
            self
        } else if let Self::Scalar(x) = self && let Self::Scalar(y) = rhs {
            Self::Scalar(x+y)
        } else if let Self::ScalarMul(x, a) = self.clone() && *a == rhs {
            Self::ScalarMul(x+1, a)
        } else if let Self::ScalarMul(x, a) = rhs.clone() && *a == self {
            Self::ScalarMul(x+1, a)
        } else if let Self::ScalarMul(x, a) = self.clone() && let Self::ScalarMul(y, b) = rhs.clone() && *a == *b{
            Self::ScalarMul(x+y, a)
        } else if self == rhs {
            Self::ScalarMul(2, Box::new(self))
        } else if let Self::Add(x) = self.clone() && let Self::Scalar(s) = rhs {
            let mut vec = x;
            let scalar = vec.drain_filter(|c| matches!(c, Self::Scalar(_))).map(|x| if let Self::Scalar(x) = x {x} else {unreachable!()}).sum::<u64>();

            vec.push(Cost::Scalar(scalar+s));
            if vec.len() == 1 {
                vec[0].clone()
            } else {
                Cost::Add(vec)
            }
        } else if let Self::Scalar(s) = self.clone() && let Self::Add(x) = rhs {
            let mut vec = x;
            let scalar = vec.drain_filter(|c| matches!(c, Self::Scalar(_))).map(|x| if let Self::Scalar(x) = x {x} else {unreachable!()}).sum::<u64>();

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

    #[test]
    fn simple_max_reduction() {
        let a = Cost::Scalar(1);
        let b = Cost::Parameter(CostParameter::SizeOf("sym".to_string()));

        assert_eq!(
            b.clone() + a.clone(),
            (a.clone() + b.clone()).max(b.clone())
        );
    }

    #[test]
    fn recursive_search_1() {
        let a = Cost::ParameterMul(
            CostParameter::ValueOf("MaxVestingSchedulesGet::get()".to_string()),
            Box::new(Cost::Parameter(CostParameter::SizeOf(
                "VestingInfo".to_string(),
            ))),
        );

        let b = (a.clone().max(Cost::Scalar(1)))
            + (Cost::Parameter(CostParameter::WritesOf("Currency::set_lock".to_string())).max(
                Cost::Parameter(CostParameter::WritesOf("Currency::remove_lock".to_string())),
            ));

        assert!(b.recursive_search(a));
    }

    #[test]
    fn max_extracts_common_scalars() {
        let a = Cost::Parameter(CostParameter::SizeOf("sym1".to_string())) + Cost::Scalar(6);
        let b = Cost::Parameter(CostParameter::SizeOf("sym2".to_string())) + Cost::Scalar(8);

        assert_eq!(
            a.max(b),
            Cost::Add(vec![
                Cost::Scalar(6),
                Cost::Max(vec![
                    Cost::Parameter(CostParameter::SizeOf("sym1".to_string())),
                    Cost::Add(vec![
                        Cost::Parameter(CostParameter::SizeOf("sym2".to_string())),
                        Cost::Scalar(2)
                    ])
                ])
            ])
        );
    }
}
