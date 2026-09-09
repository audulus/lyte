//! Match ordered native recipes against the checked loop's values and effects.
//!
//! Carry roles describe incoming values and their next-iteration roots. They do
//! not depend on declaration order or the order of equivalent scalar updates.

use super::ValueId;
use super::{LoopRegion, LoopStep};
use crate::loop_recipe::{BinaryOp, Node, Recipe};
use crate::Binop;

pub(super) struct MatchedRecipe {
    pub input: usize,
    pub output: usize,
    pub coefficients: Vec<ValueId>,
    pub carries: Vec<usize>,
}

impl LoopRegion {
    pub(super) fn match_recipe(&self, recipe: &Recipe) -> Option<MatchedRecipe> {
        if self.carries.len() != recipe.carry_count() {
            return None;
        }
        // Distinct state slots must also have distinct incoming value identities.
        if self.carries.iter().enumerate().any(|(index, carry)| {
            self.carries[..index]
                .iter()
                .any(|other| other.incoming == carry.incoming)
        }) {
            return None;
        }
        let LoopStep::StoreElement {
            stream: output,
            value,
        } = *self.tape.last()?
        else {
            return None;
        };
        self.match_recipe_carry_roles(recipe, output, value, &mut Vec::new())
    }

    fn match_recipe_carry_roles(
        &self,
        recipe: &Recipe,
        output: usize,
        result: ValueId,
        carries: &mut Vec<usize>,
    ) -> Option<MatchedRecipe> {
        if carries.len() < self.carries.len() {
            // Catalogs bound this to four carries (at most 24 permutations).
            // Trying role assignments keeps output and next-state roots equal:
            // a carry need not participate in the output calculation at all.
            for carry in 0..self.carries.len() {
                if carries.contains(&carry) {
                    continue;
                }
                carries.push(carry);
                let matched = self.match_recipe_carry_roles(recipe, output, result, carries);
                carries.pop();
                if matched.is_some() {
                    return matched;
                }
            }
            return None;
        }

        let mut bindings = vec![None; recipe.nodes.len()];
        let mut coefficients = vec![None; recipe.coefficient_count()];
        self.match_recipe_node(
            recipe,
            recipe.result,
            result,
            carries,
            &mut bindings,
            &mut coefficients,
        )?;
        for (role, node) in recipe.next_carries.iter().enumerate() {
            self.match_recipe_node(
                recipe,
                *node,
                self.carries[carries[role]].next,
                carries,
                &mut bindings,
                &mut coefficients,
            )?;
        }

        let mut input = None;
        let mut schedule = Vec::new();
        for (node, binding) in recipe.nodes.iter().zip(bindings) {
            let binding = binding?;
            match node {
                Node::Parameter(_) | Node::Carry(_) => continue,
                Node::Read => {
                    if input.replace(self.read(binding)?).is_some() {
                        return None;
                    }
                }
                Node::Binary(..) => {}
            }
            schedule.push(LoopStep::Value(binding));
        }
        schedule.push(LoopStep::StoreElement {
            stream: output,
            value: result,
        });
        self.covers(&schedule).then_some(MatchedRecipe {
            input: input?,
            output,
            coefficients: coefficients.into_iter().collect::<Option<_>>()?,
            carries: carries.clone(),
        })
    }

    fn match_recipe_node(
        &self,
        recipe: &Recipe,
        node: usize,
        value: ValueId,
        carries: &[usize],
        bindings: &mut [Option<ValueId>],
        coefficients: &mut [Option<ValueId>],
    ) -> Option<()> {
        if let Some(previous) = *bindings.get(node)? {
            return (previous == value).then_some(());
        }
        match recipe.nodes.get(node)? {
            Node::Read => {
                self.read(value)?;
            }
            Node::Parameter(role) => {
                if !self.coefficient(value) {
                    return None;
                }
                let coefficient = coefficients.get_mut(usize::from(*role))?;
                if coefficient.is_some_and(|previous| previous != value) {
                    return None;
                }
                // Different parameter roles may use the same private value.
                *coefficient = Some(value);
            }
            Node::Carry(role) => {
                let carry = self.carries.get(*carries.get(usize::from(*role))?)?;
                if carry.incoming != value {
                    return None;
                }
            }
            Node::Binary(operation, lhs_node, rhs_node) => {
                let operation = match operation {
                    BinaryOp::Add => Binop::Plus,
                    BinaryOp::Sub => Binop::Minus,
                    BinaryOp::Mul => Binop::Mult,
                };
                let (lhs, rhs) = self.binary(value, operation)?;
                self.match_recipe_node(recipe, *lhs_node, lhs, carries, bindings, coefficients)?;
                self.match_recipe_node(recipe, *rhs_node, rhs, carries, bindings, coefficients)?;
            }
        }
        bindings[node] = Some(value);
        Some(())
    }
}
