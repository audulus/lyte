//! Ordered recurrence recipes consumed by checked-loop matching and native C
//! generation. The catalog initially describes the existing one-pole and direct
//! form I biquad kernels; operand encodings remain a separate backend concern.

use crate::loop_recipe::{self, BinaryOp, Node, Recipe};

pub const MAX_COEFFICIENTS: usize = 5;
pub const MAX_CARRIES: usize = 4;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RecipeId(u8);

impl RecipeId {
    pub fn index(self) -> usize {
        usize::from(self.0)
    }

    pub fn recipe(self) -> &'static Recipe {
        &CATALOG[self.index()]
    }
}

pub const ONE_POLE: RecipeId = RecipeId(0);
pub const BIQUAD: RecipeId = RecipeId(1);

use BinaryOp::{Add, Mul, Sub};
use Node::{Binary, Carry, Parameter, Read};

const CATALOG: &[Recipe] = &[
    Recipe {
        name: "one_pole",
        // x*feed + old*feedback; output and next state share the result.
        nodes: &[
            Read,
            Parameter(0),
            Binary(Mul, 0, 1),
            Carry(0),
            Parameter(1),
            Binary(Mul, 3, 4),
            Binary(Add, 2, 5),
        ],
        result: 6,
        next_carries: &[6],
    },
    Recipe {
        name: "biquad",
        // b0*x + b1*x1 + b2*x2 - a1*y1 - a2*y2, separately rounded.
        // Carry roles are x1, x2, y1, y2; history shifts use old snapshots.
        nodes: &[
            Parameter(0),
            Read,
            Binary(Mul, 0, 1),
            Parameter(1),
            Carry(0),
            Binary(Mul, 3, 4),
            Binary(Add, 2, 5),
            Parameter(2),
            Carry(1),
            Binary(Mul, 7, 8),
            Binary(Add, 6, 9),
            Parameter(3),
            Carry(2),
            Binary(Mul, 11, 12),
            Binary(Sub, 10, 13),
            Parameter(4),
            Carry(3),
            Binary(Mul, 15, 16),
            Binary(Sub, 14, 17),
        ],
        result: 18,
        next_carries: &[1, 4, 18, 12],
    },
];

pub fn recipes() -> impl Iterator<Item = (RecipeId, &'static Recipe)> {
    CATALOG
        .iter()
        .enumerate()
        .map(|(index, recipe)| (RecipeId(index as u8), recipe))
}

pub fn validate() -> Result<(), String> {
    loop_recipe::validate_catalog(CATALOG, MAX_COEFFICIENTS, MAX_CARRIES)?;
    // The retained native adapters require these identities and arities. They
    // do not impose the arithmetic or the order of next-state updates.
    for (id, name, coefficients, carries) in
        [(ONE_POLE, "one_pole", 2, 1), (BIQUAD, "biquad", 5, 4)]
    {
        let recipe = id.recipe();
        if recipe.name != name
            || recipe.coefficient_count() != coefficients
            || recipe.carry_count() != carries
        {
            return Err(format!("{name}: incompatible native recurrence adapter"));
        }
    }
    Ok(())
}
