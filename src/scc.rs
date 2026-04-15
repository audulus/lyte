//! Tarjan's strongly-connected components, iterative form.
//!
//! Used by the `--no-recursion` safety check to find cycles in the
//! call graph, but the algorithm itself has no dependencies on the
//! compiler and is unit-tested in isolation.

/// Compute the strongly-connected components of a directed graph
/// given as an adjacency list.
///
/// Input: `adj[v]` is the list of successors of node `v`. Nodes are
/// identified by their index in `adj`.
///
/// Output: one `Vec<usize>` per SCC, each listing the node indices in
/// that component. Every node appears in exactly one SCC.
///
/// The algorithm is Tarjan's classic one-pass SCC, written iteratively
/// so it can be run against deeply nested call graphs without risking
/// a Rust stack overflow.
pub fn strongly_connected_components(adj: &[Vec<usize>]) -> Vec<Vec<usize>> {
    let n = adj.len();
    let mut index_of: Vec<i32> = vec![-1; n];
    let mut lowlink: Vec<i32> = vec![0; n];
    let mut on_stack: Vec<bool> = vec![false; n];
    let mut stack: Vec<usize> = Vec::new();
    let mut sccs: Vec<Vec<usize>> = Vec::new();
    let mut next_index: i32 = 0;

    // Each work item is (node, next adjacency slot to visit). This
    // replaces the recursive strongconnect call so we don't consume
    // the real Rust stack.
    let mut work: Vec<(usize, usize)> = Vec::new();

    for root in 0..n {
        if index_of[root] != -1 {
            continue;
        }
        index_of[root] = next_index;
        lowlink[root] = next_index;
        next_index += 1;
        stack.push(root);
        on_stack[root] = true;
        work.push((root, 0));

        while let Some(&(v, next_i)) = work.last() {
            if next_i < adj[v].len() {
                let w = adj[v][next_i];
                work.last_mut().unwrap().1 += 1;
                if index_of[w] == -1 {
                    index_of[w] = next_index;
                    lowlink[w] = next_index;
                    next_index += 1;
                    stack.push(w);
                    on_stack[w] = true;
                    work.push((w, 0));
                } else if on_stack[w] {
                    if index_of[w] < lowlink[v] {
                        lowlink[v] = index_of[w];
                    }
                }
            } else {
                if lowlink[v] == index_of[v] {
                    let mut scc = Vec::new();
                    loop {
                        let w = stack.pop().unwrap();
                        on_stack[w] = false;
                        scc.push(w);
                        if w == v {
                            break;
                        }
                    }
                    sccs.push(scc);
                }
                work.pop();
                if let Some(&(parent, _)) = work.last() {
                    if lowlink[v] < lowlink[parent] {
                        lowlink[parent] = lowlink[v];
                    }
                }
            }
        }
    }

    sccs
}

/// Returns true if `scc` describes a true cycle: either it has more
/// than one member (a multi-node cycle) or its single member has a
/// self-edge in `adj` (a self-loop).
pub fn scc_is_cycle(scc: &[usize], adj: &[Vec<usize>]) -> bool {
    if scc.len() > 1 {
        return true;
    }
    let v = scc[0];
    adj[v].contains(&v)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    /// Convert each SCC to a sorted Vec and collect into a HashSet of
    /// BTree-like representations so comparisons don't depend on the
    /// order Tarjan happens to emit components in.
    fn canonical(sccs: Vec<Vec<usize>>) -> HashSet<Vec<usize>> {
        sccs.into_iter()
            .map(|mut s| {
                s.sort();
                s
            })
            .collect()
    }

    fn expected(sets: &[&[usize]]) -> HashSet<Vec<usize>> {
        sets.iter()
            .map(|s| {
                let mut v = s.to_vec();
                v.sort();
                v
            })
            .collect()
    }

    #[test]
    fn empty_graph() {
        let adj: Vec<Vec<usize>> = vec![];
        assert!(strongly_connected_components(&adj).is_empty());
    }

    #[test]
    fn single_node_no_edges() {
        let adj = vec![vec![]];
        let sccs = strongly_connected_components(&adj);
        assert_eq!(canonical(sccs), expected(&[&[0]]));
    }

    #[test]
    fn single_node_self_loop() {
        let adj = vec![vec![0]];
        let sccs = strongly_connected_components(&adj);
        assert_eq!(canonical(sccs.clone()), expected(&[&[0]]));
        assert!(scc_is_cycle(&sccs[0], &adj));
    }

    #[test]
    fn single_node_no_self_loop_not_a_cycle() {
        let adj = vec![vec![]];
        let sccs = strongly_connected_components(&adj);
        assert!(!scc_is_cycle(&sccs[0], &adj));
    }

    #[test]
    fn two_nodes_acyclic() {
        // 0 → 1, no edge back.
        let adj = vec![vec![1], vec![]];
        let sccs = strongly_connected_components(&adj);
        assert_eq!(canonical(sccs), expected(&[&[0], &[1]]));
    }

    #[test]
    fn two_nodes_mutual_edge() {
        // 0 ↔ 1 forms a single SCC.
        let adj = vec![vec![1], vec![0]];
        let sccs = strongly_connected_components(&adj);
        assert_eq!(canonical(sccs.clone()), expected(&[&[0, 1]]));
        assert!(scc_is_cycle(&sccs[0], &adj));
    }

    #[test]
    fn three_node_cycle() {
        // 0 → 1 → 2 → 0.
        let adj = vec![vec![1], vec![2], vec![0]];
        let sccs = strongly_connected_components(&adj);
        assert_eq!(canonical(sccs), expected(&[&[0, 1, 2]]));
    }

    #[test]
    fn dag() {
        // 0 → 1, 0 → 2, 1 → 3, 2 → 3.
        let adj = vec![vec![1, 2], vec![3], vec![3], vec![]];
        let sccs = strongly_connected_components(&adj);
        assert_eq!(canonical(sccs), expected(&[&[0], &[1], &[2], &[3]]));
    }

    #[test]
    fn three_disjoint_cycles_with_bridges() {
        // Three non-trivial SCCs connected by one-way bridges:
        //   {0,1,2}: 0→1→2→0
        //   {3,4}:   3→4→3
        //   {5,6,7}: 5→6→7→5
        // Plus forward-only bridges 1→3 and 3→5 so the graph is
        // connected but the SCCs stay distinct.
        let adj = vec![
            vec![1],    // 0
            vec![2, 3], // 1
            vec![0],    // 2
            vec![4, 5], // 3
            vec![3],    // 4
            vec![6],    // 5
            vec![7],    // 6
            vec![5],    // 7
        ];
        let sccs = strongly_connected_components(&adj);
        assert_eq!(
            canonical(sccs),
            expected(&[&[0, 1, 2], &[3, 4], &[5, 6, 7]])
        );
    }

    #[test]
    fn disconnected_components() {
        // Two independent cycles that share no edges.
        // 0 ↔ 1 and 2 ↔ 3.
        let adj = vec![vec![1], vec![0], vec![3], vec![2]];
        let sccs = strongly_connected_components(&adj);
        assert_eq!(canonical(sccs), expected(&[&[0, 1], &[2, 3]]));
    }

    #[test]
    fn self_loop_plus_incoming_edge() {
        // 0 → 1, 1 → 1. Node 1 has a self-loop but 0 does not participate.
        let adj = vec![vec![1], vec![1]];
        let sccs = strongly_connected_components(&adj);
        let canon = canonical(sccs.clone());
        assert_eq!(canon, expected(&[&[0], &[1]]));

        // Find which component is {1} and verify it's a cycle.
        let cycle_scc: &Vec<usize> = sccs.iter().find(|s| s.contains(&1)).unwrap();
        assert!(scc_is_cycle(cycle_scc, &adj));

        // And {0} alone is not a cycle.
        let noncycle_scc: &Vec<usize> = sccs.iter().find(|s| s.contains(&0)).unwrap();
        assert!(!scc_is_cycle(noncycle_scc, &adj));
    }

    #[test]
    fn every_node_covered_exactly_once() {
        // Regression guard: a randomish-looking graph.
        let adj = vec![
            vec![1, 2],
            vec![3],
            vec![1, 4],
            vec![0],
            vec![5],
            vec![4, 6],
            vec![],
        ];
        let sccs = strongly_connected_components(&adj);
        let mut seen: Vec<usize> = sccs.iter().flat_map(|s| s.iter().copied()).collect();
        seen.sort();
        assert_eq!(seen, vec![0, 1, 2, 3, 4, 5, 6]);
    }
}
