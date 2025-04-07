mod flatten;
mod node;
mod tree;

pub type TreeFloat = f64;

pub use flatten::{Edge, Edges, flatten_tree};
pub use node::{Node, NodeId, NodeType};
pub use tree::Tree;
