// #![cfg_attr(
//     debug_assertions,
//     allow(
//         dead_code,
//         unused_imports,
//         unused_variables,
//         unused_assignments,
//         unused_mut,
//         clippy::needless_return,
//         clippy::collapsible_if,
//         clippy::collapsible_match,
//         clippy::derivable_impls,
//         clippy::too_many_arguments,
//         clippy::type_complexity,
//     )
// )]

mod newick;
mod phylo;

pub use newick::parse_newick;
pub use phylo::{Edge, Edges, Node, NodeId, NodeType, Tree, TreeFloat, flatten_tree};
