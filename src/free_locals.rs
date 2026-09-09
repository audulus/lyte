//! Capture discovery needs lexical bindings, not solved types or publication.
use crate::{CheckedBody, ExprID, LocalId, Reference};
use std::collections::HashSet;

/// The narrow facts needed from either a checked body or a check in progress.
pub(crate) struct BindingNode {
    pub children: Vec<ExprID>,
    pub used: Option<LocalId>,
    pub declared: Vec<LocalId>,
    /// False for unavailable resolution/binder facts. Known facts remain useful.
    pub complete: bool,
}

pub(crate) trait BindingFacts {
    fn binding_node(&self, id: ExprID) -> BindingNode;
}

pub(crate) struct FreeLocals {
    /// Unique free locals, in first-use order, including uses in nested lambdas.
    pub locals: Vec<LocalId>,
    /// An empty list establishes noncapture only when discovery is complete.
    pub complete: bool,
}

pub(crate) fn free_locals(
    facts: &impl BindingFacts,
    root: ExprID,
    bound: impl IntoIterator<Item = LocalId>,
) -> FreeLocals {
    fn walk(
        facts: &impl BindingFacts,
        id: ExprID,
        declared: &mut HashSet<LocalId>,
        result: &mut FreeLocals,
    ) {
        let node = facts.binding_node(id);
        result.complete &= node.complete;
        result.locals.extend(node.used);
        declared.extend(node.declared);
        for child in node.children {
            walk(facts, child, declared, result);
        }
    }
    let mut declared = bound.into_iter().collect();
    let mut result = FreeLocals {
        locals: vec![],
        complete: true,
    };
    walk(facts, root, &mut declared, &mut result);
    let mut seen = HashSet::new();
    result
        .locals
        .retain(|local| !declared.contains(local) && seen.insert(*local));
    result
}

impl BindingFacts for CheckedBody {
    fn binding_node(&self, id: ExprID) -> BindingNode {
        BindingNode {
            children: self[id].subexprs(),
            used: match self.reference(id) {
                Some(Reference::Local(local)) => Some(*local),
                _ => None,
            },
            declared: self.binders(id).to_vec(),
            complete: true,
        }
    }
}
