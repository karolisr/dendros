// -------------------------------------
// #![allow(dead_code)]
// #![allow(unused_mut)]
// #![allow(unused_imports)]
// #![allow(unused_variables)]
// #![allow(unused_assignments)]
// #![allow(clippy::single_match)]
// #![allow(clippy::collapsible_if)]
// #![allow(clippy::derivable_impls)]
// #![allow(clippy::type_complexity)]
// #![allow(clippy::collapsible_match)]
// #![allow(clippy::too_many_arguments)]
// #![allow(clippy::vec_init_then_push)]
// #![allow(clippy::needless_range_loop)]
// -------------------------------------

mod newick;
mod nexus;
mod phylo;

pub use newick::{parse_newick, write_newick};
pub use nexus::{NexusError, parse_nexus, parse_nexus_advanced};
pub use phylo::{
    Attribute, AttributeType, AttributeValue, AttributeValueType, Edge,
    LttPoint, Node, NodeId, NodeType, Tree, TreeError, TreeFloat, TreeInt, ltt,
};
