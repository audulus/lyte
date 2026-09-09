//! Ordered pointwise recipes shared by checked-loop matching and C generation.
//!
//! This module is deliberately independent of compiler types: build.rs reads the
//! same graph that runtime selection matches. Nodes are SSA values in evaluation
//! order, not algebraic expressions to reassociate. The finite catalog bounds
//! precompiled code growth; it is not interpreted during sample processing.

pub const MAX_COEFFICIENTS: usize = 4;

pub use crate::loop_recipe::{BinaryOp, Node, Recipe};

/// A catalog identity; construction only occurs through the shared catalog.
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

/// The affine map recipe `input * gain + bias`.
pub const AFFINE: RecipeId = RecipeId(0);

use BinaryOp::{Add, Mul, Sub};
use Node::{Binary, Parameter, Read};

const CATALOG: &[Recipe] = &[
    Recipe {
        name: "affine_map",
        // x*g+b
        nodes: &[
            Read,
            Parameter(0),
            Binary(Mul, 0, 1),
            Parameter(1),
            Binary(Add, 2, 3),
        ],
        result: 4,
        next_carries: &[],
    },
    Recipe {
        name: "offset_scale",
        // (x-b)*g: subtraction precedes multiplication.
        nodes: &[
            Read,
            Parameter(0),
            Binary(Sub, 0, 1),
            Parameter(1),
            Binary(Mul, 2, 3),
        ],
        result: 4,
        next_carries: &[],
    },
    Recipe {
        name: "cubic",
        // x-k*((x*x)*x), with one read reused by every occurrence of x.
        nodes: &[
            Read,
            Parameter(0),
            Binary(Mul, 0, 0),
            Binary(Mul, 2, 0),
            Binary(Mul, 1, 3),
            Binary(Sub, 0, 4),
        ],
        result: 5,
        next_carries: &[],
    },
    Recipe {
        name: "horner",
        // ((a*x+b)*x+c)*x+d, preserving each multiply/add rounding.
        nodes: &[
            Read,
            Parameter(0),
            Binary(Mul, 1, 0),
            Parameter(1),
            Binary(Add, 2, 3),
            Binary(Mul, 4, 0),
            Parameter(2),
            Binary(Add, 5, 6),
            Binary(Mul, 7, 0),
            Parameter(3),
            Binary(Add, 8, 9),
        ],
        result: 10,
        next_carries: &[],
    },
    Recipe {
        name: "smootherstep",
        // ((x*x)*x) * (x*(x*a-b)+c); a=6, b=15, c=10 gives
        // quintic smootherstep for normalized x, without clamping.
        nodes: &[
            Read,
            Binary(Mul, 0, 0),
            Binary(Mul, 1, 0),
            Parameter(0),
            Binary(Mul, 0, 3),
            Parameter(1),
            Binary(Sub, 4, 5),
            Binary(Mul, 0, 6),
            Parameter(2),
            Binary(Add, 7, 8),
            Binary(Mul, 2, 9),
        ],
        result: 10,
        next_carries: &[],
    },
    Recipe {
        name: "gain",
        nodes: &[Read, Parameter(0), Binary(Mul, 0, 1)],
        result: 2,
        next_carries: &[],
    },
    Recipe {
        name: "offset",
        nodes: &[Read, Parameter(0), Binary(Add, 0, 1)],
        result: 2,
        next_carries: &[],
    },
    Recipe {
        name: "square",
        // Both operands reuse the same sampled input value.
        nodes: &[Read, Binary(Mul, 0, 0)],
        result: 1,
        next_carries: &[],
    },
];

pub fn recipes() -> impl Iterator<Item = (RecipeId, &'static Recipe)> {
    CATALOG
        .iter()
        .enumerate()
        .map(|(index, recipe)| (RecipeId(index as u8), recipe))
}

/// Validate the shared semantics against the existing pointwise encoding.
pub fn validate() -> Result<(), String> {
    crate::loop_recipe::validate_catalog(CATALOG, MAX_COEFFICIENTS, 0)?;
    if AFFINE.recipe().name != "affine_map"
        || CATALOG
            .iter()
            .any(|recipe| recipe.result != recipe.nodes.len() - 1)
    {
        return Err("invalid pointwise catalog identities or terminal result".into());
    }
    Ok(())
}
