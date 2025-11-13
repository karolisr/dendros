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

mod parsers;
mod phylo;

pub type TreeFloat = f64;
pub type TreeInt = i64;

pub use parsers::TreeParseError;
pub use parsers::newick::parse_newick;
pub use parsers::newick::write_newick;
pub use parsers::nexus::NexusError;
pub use parsers::nexus::NexusFile;
pub use parsers::nexus::parse_nexus;
pub use parsers::nexus::parse_nexus_advanced;
pub use parsers::parse_trees;
pub use phylo::attribute::Attribute;
pub use phylo::attribute::AttributeType;
pub use phylo::attribute::AttributeValue;
pub use phylo::attribute::AttributeValueType;
pub use phylo::edges::Edge;
pub use phylo::ltt::LttPoint;
pub use phylo::ltt::ltt;
pub use phylo::node::Node;
pub use phylo::node::NodeId;
pub use phylo::tree::Tree;
pub use phylo::tree::TreeError;
