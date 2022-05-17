use rustc_middle::mir::Location;
use rustc_mir_dataflow::{fmt::DebugWithContext, lattice::JoinSemiLattice};
use rustc_target::abi::VariantIdx;
use std::collections::HashMap;

#[derive(Eq, PartialEq, Clone, Debug)]
pub(crate) enum Variants {
    Variant(VariantIdx),
    Or(Box<Variants>, Box<Variants>),
}

impl Variants {
    pub fn or(&self, other: &Variants) -> Self {
        if *self == *other {
            (*self).clone()
        } else {
            Variants::Or(Box::new((*self).clone()), Box::new((*other).clone()))
        }
    }

    pub fn flatten_or(&self) -> Vec<VariantIdx> {
        match self {
            Variants::Variant(variant_id) => vec![*variant_id],
            Variants::Or(a, b) => {
                let mut flat_a = a.flatten_or();
                let mut flat_b = b.flatten_or();

                flat_a.append(&mut flat_b);

                flat_a
            }
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Default)]
pub(crate) struct EventVariantsDomain {
    event_variant_at_location: HashMap<Location, Variants>,
}

impl EventVariantsDomain {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_variant(&mut self, location: Location, variant_id: VariantIdx) {
        if let std::collections::hash_map::Entry::Vacant(e) =
            self.event_variant_at_location.entry(location)
        {
            e.insert(Variants::Variant(variant_id));
        } else {
            // Since we have no loop or recursion, location should be visited at most once
            unreachable!();
        }
    }

    pub fn get(&self, location: Location) -> Option<&Variants> {
        self.event_variant_at_location.get(&location)
    }

    pub fn is_empty(&self) -> bool {
        self.event_variant_at_location.is_empty()
    }
}

impl JoinSemiLattice for EventVariantsDomain {
    fn join(&mut self, other: &Self) -> bool {
        let mut res = false;

        for (k, v) in other.event_variant_at_location.iter() {
            if self.event_variant_at_location.contains_key(k) {
                let current_value_self = self.event_variant_at_location.get(k).unwrap();

                if *v != *current_value_self {
                    self.event_variant_at_location
                        .insert(*k, current_value_self.or(v));
                    res = true;
                }
            } else {
                self.event_variant_at_location.insert(*k, (*v).clone());
                res = true;
            }
        }

        res
    }
}

impl<C> DebugWithContext<C> for EventVariantsDomain {}
