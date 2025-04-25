mod flatten;
mod ltt;
mod node;
mod tree;

pub type TreeFloat = f64;

pub use flatten::{Edge, Edges, chunk_edges, flatten_tree};
pub use ltt::{LttPoint, ltt};
pub use node::{Node, NodeId, NodeType};
pub use tree::Tree;
