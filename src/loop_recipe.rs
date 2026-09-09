//! Compiler-independent ordered values shared by native loop matching and C
//! generation. Carry updates are simultaneous next-state roots, not assignments
//! to execute in role order. The output is the single terminal memory effect.

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Node {
    Read,
    Parameter(u8),
    Carry(u8),
    Binary(BinaryOp, usize, usize),
}

#[derive(Clone, Copy, Debug)]
pub struct Recipe {
    pub name: &'static str,
    pub nodes: &'static [Node],
    /// Value written by the terminal output effect, after all ordered work.
    pub result: usize,
    /// Final value for each incoming carry role. All roots refer to the same
    /// iteration's immutable values, including its incoming carry snapshots.
    pub next_carries: &'static [usize],
}

impl Recipe {
    pub fn coefficient_count(&self) -> usize {
        self.nodes
            .iter()
            .filter_map(|node| match node {
                Node::Parameter(role) => Some(usize::from(*role) + 1),
                _ => None,
            })
            .max()
            .unwrap_or(0)
    }

    pub fn carry_count(&self) -> usize {
        self.next_carries.len()
    }
}

/// Validate semantic recipes before either consumer uses them. Physical operand
/// encodings supply their own limits. No work node may be unused: native C could
/// remove it while the matcher promises coverage of the complete ordered tape.
pub fn validate_catalog(
    catalog: &[Recipe],
    max_coefficients: usize,
    max_carries: usize,
) -> Result<(), String> {
    use Node::*;
    if catalog.len() > usize::from(u8::MAX) + 1 {
        return Err("native recipe catalog exceeds identity range".into());
    }
    let mut names = std::collections::HashSet::new();
    for recipe in catalog {
        let valid_name = recipe
            .name
            .bytes()
            .all(|c| c.is_ascii_lowercase() || c == b'_');
        if recipe.name.is_empty() || !valid_name || !names.insert(recipe.name) {
            return Err(format!("invalid or duplicate recipe name: {}", recipe.name));
        }
        if recipe.result >= recipe.nodes.len()
            || recipe
                .next_carries
                .iter()
                .any(|&next| next >= recipe.nodes.len())
            || recipe.carry_count() > max_carries
        {
            return Err(format!("{}: invalid output or carry roots", recipe.name));
        }
        let mut reads = 0;
        let mut parameters = vec![false; max_coefficients];
        let mut carries = vec![false; recipe.carry_count()];
        for (index, node) in recipe.nodes.iter().enumerate() {
            let (roles, role, label) = match *node {
                Read => {
                    reads += 1;
                    continue;
                }
                Parameter(role) => (&mut parameters, usize::from(role), "parameter"),
                Carry(role) => (&mut carries, usize::from(role), "carry"),
                Binary(_, left, right) => {
                    if left >= index || right >= index {
                        return Err(format!("{}: operand is not an earlier value", recipe.name));
                    }
                    continue;
                }
            };
            if role >= roles.len() || roles[role] {
                return Err(format!("{}: invalid {label} role", recipe.name));
            }
            roles[role] = true;
        }
        if reads != 1
            || parameters[..recipe.coefficient_count()].contains(&false)
            || carries.contains(&false)
        {
            return Err(format!(
                "{}: expected one read and complete parameter/carry roles",
                recipe.name
            ));
        }
        let mut live = vec![false; recipe.nodes.len()];
        live[recipe.result] = true;
        for &next in recipe.next_carries {
            live[next] = true;
        }
        for index in (0..recipe.nodes.len()).rev() {
            if live[index] {
                if let Binary(_, left, right) = recipe.nodes[index] {
                    live[left] = true;
                    live[right] = true;
                }
            }
        }
        if live.contains(&false) {
            return Err(format!(
                "{}: unused node would violate work coverage",
                recipe.name
            ));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use Node::*;

    #[test]
    fn output_and_next_state_are_independent_roots_with_complete_roles() {
        let recipe = Recipe {
            name: "state_roots",
            nodes: &[
                Read,
                Carry(0),
                Carry(1),
                Binary(BinaryOp::Add, 0, 1),
                Binary(BinaryOp::Mul, 2, 3),
            ],
            result: 0,
            next_carries: &[3, 4],
        };
        assert!(validate_catalog(&[recipe], 0, 2).is_ok());
        for invalid in [
            Recipe {
                next_carries: &[3, 5],
                ..recipe
            },
            Recipe {
                next_carries: &[3],
                ..recipe
            },
            Recipe {
                next_carries: &[3, 2],
                ..recipe
            },
            Recipe {
                nodes: &[Read, Carry(0), Carry(0)],
                result: 0,
                next_carries: &[1, 2],
                ..recipe
            },
            Recipe {
                nodes: &[Read, Carry(0), Binary(BinaryOp::Add, 0, 3)],
                result: 0,
                next_carries: &[2],
                ..recipe
            },
        ] {
            assert!(validate_catalog(&[invalid], 0, 2).is_err());
        }
    }
}
