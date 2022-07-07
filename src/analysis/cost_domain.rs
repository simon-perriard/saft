use crate::analysis::cost_language::Cost;
use core::fmt;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{Body, Local, Place, ProjectionElem};
use rustc_middle::ty::{Ty, TyCtxt, TyKind};
use rustc_mir_dataflow::{fmt::DebugWithContext, lattice::JoinSemiLattice};
use std::ops::{Deref, DerefMut};

use super::cost_language::cost_to_big_o;

#[derive(Eq, PartialEq, Clone, Debug, Default)]
pub(crate) struct LocalsInfo<'tcx>(IndexVec<Local, LocalInfo<'tcx>>);

impl<'tcx> Deref for LocalsInfo<'tcx> {
    type Target = IndexVec<Local, LocalInfo<'tcx>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'tcx> DerefMut for LocalsInfo<'tcx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'tcx> LocalsInfo<'tcx> {
    pub fn inter_join(&mut self, _other: &Self) {
        // nothing to join for now we only carry type info
    }

    pub fn get_type_info_for_place(&self, place: &Place) -> Option<TypeInfo<'tcx>> {
        let Place { local, projection } = place;

        if projection.is_empty() {
            // No projection, return the outermost LocalInfo
            Some(self[*local].type_info.clone())
        } else {
            // Go down the projections
            let mut current_local_info = self[*local].type_info.clone();

            for (_, proj) in place.iter_projections() {
                match proj {
                    ProjectionElem::Field(field, _) => {
                        let field_local_info = current_local_info.get_member(field.index());

                        if let Some(field_local_info) = field_local_info {
                            current_local_info = field_local_info.clone();
                        } else {
                            return None;
                        }
                    }
                    ProjectionElem::Deref => {
                        // References should already be abstracted away, just continue
                    }
                    ProjectionElem::Downcast(_, variant_idx) => {
                        // Downcast if a field is available
                        let fields = current_local_info.get_members();
                        let maybe_field = fields.get(variant_idx.index());

                        if let Some(field_local_info) = maybe_field {
                            current_local_info = field_local_info.clone();
                        }
                    }
                    _ => {
                        panic!("{:#?}", proj)
                    }
                }
            }
            Some(current_local_info)
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct LocalInfo<'tcx> {
    // TODO:
    // keep additional set of abstract values, one per local
    // in each of them we keep symbolic values and attributes
    pub type_info: TypeInfo<'tcx>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct TypeInfo<'tcx> {
    ty: Vec<Ty<'tcx>>,
    members: Vec<TypeInfo<'tcx>>,
}

impl<'tcx> TypeInfo<'tcx> {
    pub fn new(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        match ty.kind() {
            TyKind::Adt(adt_def, substs) => {
                if adt_def.is_enum() {
                    let variants = adt_def
                        .variants()
                        .iter()
                        .map(|variant_def| {
                            let variant_ty = tcx.type_of(variant_def.def_id);
                            let variant_fields = variant_def
                                .fields
                                .iter()
                                .map(|field| TypeInfo::new(field.ty(tcx, substs), tcx))
                                .collect::<Vec<_>>();
                            TypeInfo {
                                ty: vec![variant_ty],
                                members: variant_fields,
                            }
                        })
                        .collect::<Vec<_>>();

                    TypeInfo {
                        ty: vec![ty],
                        members: variants,
                    }
                } else {
                    let fields = adt_def
                        .all_fields()
                        .map(|field| TypeInfo::new(field.ty(tcx, substs), tcx))
                        .collect::<Vec<_>>();
                    TypeInfo {
                        ty: vec![ty],
                        members: fields,
                    }
                }
            }
            // For closures, taken from https://doc.rust-lang.org/nightly/nightly-rustc/rustc_borrowck/type_check/struct.TypeVerifier.html#method.field_ty
            TyKind::Closure(_, substs) => {
                let upvars = substs
                    .as_closure()
                    .tupled_upvars_ty()
                    .tuple_fields()
                    .iter()
                    .map(|ty| TypeInfo::new(ty, tcx))
                    .collect::<Vec<_>>();

                TypeInfo {
                    ty: vec![ty],
                    members: upvars,
                }
            }
            TyKind::Tuple(list_ty) => {
                let members = list_ty
                    .iter()
                    .map(|ty| TypeInfo::new(ty, tcx))
                    .collect::<Vec<_>>();

                TypeInfo {
                    ty: vec![ty],
                    members,
                }
            }
            TyKind::Ref(_, ty, _) => Self::new(*ty, tcx),
            TyKind::Projection(projection_ty) => {
                let members = TypeInfo::new(projection_ty.self_ty(), tcx).members;
                TypeInfo {
                    ty: vec![ty],
                    members,
                }
            }
            _ => TypeInfo {
                ty: vec![ty],
                members: Vec::new(),
            },
        }
    }

    pub fn has_members(&self) -> bool {
        !self.members.is_empty()
    }

    pub fn get_ty(&self) -> Ty<'tcx> {
        *self.ty.last().unwrap()
    }

    pub fn set_ty(&mut self, ty: Ty<'tcx>) {
        if !self.ty.contains(&ty) {
            self.ty.push(ty);
        }
    }

    pub fn get_member(&self, index: usize) -> Option<&TypeInfo<'tcx>> {
        self.members.get(index)
    }

    pub fn get_members(&self) -> Vec<TypeInfo<'tcx>> {
        self.members.clone()
    }

    pub fn set_member(&mut self, index: usize, type_info: TypeInfo<'tcx>) {
        self.members.remove(index);
        self.members.insert(index, type_info);
    }
}

impl<'tcx> LocalInfo<'tcx> {
    pub fn new(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> Self {
        LocalInfo {
            type_info: TypeInfo::new(ty, tcx),
        }
    }

    pub fn from_type_info(type_info: TypeInfo<'tcx>) -> Self {
        LocalInfo { type_info }
    }

    pub fn has_fields(&self) -> bool {
        self.type_info.has_members()
    }

    pub fn get_ty(&self) -> Ty<'tcx> {
        self.type_info.get_ty()
    }

    pub fn set_ty(&mut self, ty: Ty<'tcx>) {
        self.type_info.set_ty(ty);
    }

    pub fn set_type_info(&mut self, info: TypeInfo<'tcx>) {
        self.set_ty(info.get_ty());

        if self.type_info.members.is_empty() {
            self.type_info.members = info.members.clone();
        }
    }

    pub fn get_member(&self, index: usize) -> Option<&TypeInfo<'tcx>> {
        self.type_info.get_member(index)
    }

    pub fn get_members(&self) -> Vec<TypeInfo<'tcx>> {
        self.type_info.get_members()
    }

    pub fn set_member(&mut self, index: usize, type_info: TypeInfo<'tcx>) {
        self.type_info.set_member(index, type_info)
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Default)]
pub(crate) struct CostDomain {
    bytes_read: Cost,
    bytes_written: Cost,
    bytes_deposited: Cost,
    steps_executed: Cost,
}

#[derive(Eq, PartialEq, Clone, Debug, Default)]
pub(crate) struct ExtendedCostAnalysisDomain<'tcx> {
    pub costs: CostDomain,
    pub locals_info: LocalsInfo<'tcx>,
}

impl<'tcx> ExtendedCostAnalysisDomain<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>, body: &Body<'tcx>) -> Self {
        ExtendedCostAnalysisDomain {
            costs: CostDomain::new(),
            locals_info: Self::get_local_infos(tcx, body),
        }
    }

    fn get_local_infos(tcx: TyCtxt<'tcx>, body: &Body<'tcx>) -> LocalsInfo<'tcx> {
        // Fill the map with current body type

        let locals_info: IndexVec<Local, LocalInfo<'tcx>> = body
            .local_decls
            .iter_enumerated()
            .map(|(_, local_decl)| LocalInfo::new(local_decl.ty, tcx))
            .collect();

        LocalsInfo(locals_info)
    }

    pub fn add_reads(&mut self, size: Cost) {
        self.costs.add_reads(size);
    }

    pub fn add_writes(&mut self, size: Cost) {
        self.costs.add_writes(size);
    }

    pub fn add_events(&mut self, size: Cost) {
        self.costs.add_events(size);
    }

    pub fn add_steps(&mut self, size: Cost) {
        self.costs.add_steps(size);
    }

    pub fn add_step(&mut self) {
        self.costs.add_step();
    }

    pub fn cost_big_o_mul(&mut self, mul_factor: Cost) {
        self.costs.cost_big_o_mul(mul_factor);
    }

    pub fn get_type_info_for_place(&self, place: &Place) -> Option<TypeInfo<'tcx>> {
        self.locals_info.get_type_info_for_place(place)
    }

    pub fn override_with_caller_type_context(
        &mut self,
        caller_context_args_type_info: &Vec<TypeInfo<'tcx>>,
    ) {
        let mut idx = Local::from_usize(1);

        for caller_context_arg_type_info in caller_context_args_type_info.iter() {
            self.locals_info[idx].type_info = (*caller_context_arg_type_info).clone();
            idx = idx + 1;
        }
    }

    pub fn inter_join(&mut self, other: &Self) {
        self.costs.inter_join(&other.costs);

        self.locals_info.inter_join(&other.locals_info);
    }
}

impl<'tcx> JoinSemiLattice for ExtendedCostAnalysisDomain<'tcx> {
    fn join(&mut self, other: &Self) -> bool {
        self.costs.join(&other.costs) && self.locals_info.join(&other.locals_info)
    }
}

impl<'tcx, C> DebugWithContext<C> for ExtendedCostAnalysisDomain<'tcx> {}

impl<'tcx> fmt::Display for ExtendedCostAnalysisDomain<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.costs)
    }
}

impl CostDomain {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_reads(&mut self, size: Cost) {
        self.bytes_read = self.bytes_read.clone() + size;
    }

    pub fn add_writes(&mut self, size: Cost) {
        self.bytes_written = self.bytes_written.clone() + size;
    }

    pub fn add_events(&mut self, size: Cost) {
        self.bytes_deposited = self.bytes_deposited.clone() + size;
    }

    pub fn add_steps(&mut self, steps: Cost) {
        self.steps_executed = self.steps_executed.clone() + steps;
    }

    pub fn add_step(&mut self) {
        self.steps_executed = self.steps_executed.clone() + Cost::Scalar(1);
    }

    pub fn cost_big_o_mul(&mut self, mul_factor: Cost) {
        self.bytes_read = mul_factor
            .clone()
            .mul(cost_to_big_o(self.bytes_read.clone()));
        self.bytes_written = mul_factor
            .clone()
            .mul(cost_to_big_o(self.bytes_written.clone()));
        self.bytes_deposited = mul_factor
            .clone()
            .mul(cost_to_big_o(self.bytes_deposited.clone()));
        self.steps_executed = mul_factor.mul(cost_to_big_o(self.steps_executed.clone()));
    }

    pub fn inter_join(&mut self, other: &Self) {
        self.bytes_read = self.bytes_read.clone() + other.bytes_read.clone();
        self.bytes_written = self.bytes_written.clone() + other.bytes_written.clone();
        self.bytes_deposited = self.bytes_deposited.clone() + other.bytes_deposited.clone();
        self.steps_executed = self.steps_executed.clone() + other.steps_executed.clone();
    }
}

impl JoinSemiLattice for CostDomain {
    fn join(&mut self, other: &Self) -> bool {
        if other.bytes_read.is_zero()
            && other.bytes_written.is_zero()
            && other.bytes_deposited.is_zero()
            && other.steps_executed.is_zero()
            || self.bytes_read == other.bytes_read
                && self.bytes_written == other.bytes_written
                && self.bytes_deposited == other.bytes_deposited
                && self.steps_executed == other.steps_executed
        {
            false
        } else {
            self.bytes_read = self.bytes_read.max(other.bytes_read.clone());
            self.bytes_written = self.bytes_written.max(other.bytes_written.clone());
            self.bytes_deposited = self.bytes_deposited.max(other.bytes_deposited.clone());
            self.steps_executed = self.steps_executed.max(other.steps_executed.clone());
            true
        }
    }
}

impl<C> DebugWithContext<C> for CostDomain {}

impl fmt::Display for CostDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "== bytes read ==\n{}\n\n== bytes written ==\n{}\n\n== bytes deposited ==\n{}\n\n== steps executed ==\n{}\n",
            self.bytes_read,
            self.bytes_written,
            self.bytes_deposited,
            self.steps_executed.reduce_add_chain()
        )
    }
}

impl<'tcx> JoinSemiLattice for LocalsInfo<'tcx> {
    fn join(&mut self, other: &Self) -> bool {
        // Locals are in the same order since we are in the same function
        let mut res = false;

        for (local, other_local_info) in other.iter_enumerated() {
            let local_join_res = self[local].join(other_local_info);
            res |= local_join_res;
        }
        res
    }
}

impl<'tcx> JoinSemiLattice for LocalInfo<'tcx> {
    fn join(&mut self, other: &Self) -> bool {
        self.type_info.join(&other.type_info)
    }
}

impl<'tcx> JoinSemiLattice for TypeInfo<'tcx> {
    fn join(&mut self, other: &Self) -> bool {
        if self == other {
            // no change
            return false;
        }

        let mut members_changed = false;

        if self.members.len() == other.members.len() {
            for (self_field, other_field) in self.members.iter_mut().zip(other.members.clone()) {
                // recursively join
                members_changed |= self_field.join(&other_field);
            }
        } else {
            // This is ok for complex types since we allow the full specialization
            // the types must have the same history (prefix)
            if self.ty[0] == other.ty[0] {
                if self.ty.len() < other.ty.len() {
                    self.members = other.members.clone();
                    members_changed |= true;
                } else if self.ty.len() > other.ty.len() {
                    //Other is less precise
                    members_changed |= false;
                } else {
                    panic!("Should be the same type:\nSELF:\n{:#?}\nOTHER\n{:#?}", self, other);
                }
            } else {
                // Trait specialization
                self.set_ty(other.get_ty());
                self.members = other.members.clone();
                return true;
            }
        }

        if self.get_ty() == other.get_ty() {
            // we have the same higher type info
            // join will depend on the fields
            false || members_changed
        } else if self.ty.len() > other.ty.len() {
            // we already have a more precise information
            false || members_changed
        } else if self.ty.len() < other.ty.len() {
            // other is more informative
            self.ty = other.ty.clone();
            true
        } else {
            //COND: self.ty.len() == other.ty.len()
            // but final type is not the same
            // Example:
            /*
                fn foo<A,B>(c: bool) -> Bar
                    where   A: Bar,
                            B: Bar
                {
                    let res: Bar;

                    let a: A = new A;
                    let b: B = new B;

                    if c {
                        res = a;
                    } else {
                        res = b;
                    }

                    res
                }
            */

            panic!(
                "SOUNDNESS BREAKS: {:#?} --- {:#?}",
                self.get_ty().kind(),
                other.get_ty().kind()
            );
        }
    }
}
