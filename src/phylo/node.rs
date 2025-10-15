use super::{TreeFloat, TreeInt};
use slotmap::new_key_type;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    str::FromStr,
    sync::Arc,
};

new_key_type! { pub struct NodeId; }

#[derive(Clone, PartialEq)]
pub enum Attribute {
    Text(String),
    Decimal(TreeFloat),
    Integer(TreeInt),
    Range(TreeFloat, TreeFloat),
}

impl Debug for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Text(arg0) => f.debug_tuple("Text").field(arg0).finish(),
            Self::Decimal(arg0) => {
                f.debug_tuple("Decimal").field(arg0).finish()
            }
            Self::Integer(arg0) => {
                f.debug_tuple("Integer").field(arg0).finish()
            }
            Self::Range(arg0, arg1) => {
                f.debug_tuple("Range").field(arg0).field(arg1).finish()
            }
        }
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Attribute::Text(text) => text.to_string(),
                Attribute::Decimal(decimal) => format!("{decimal}"),
                Attribute::Integer(integer) => format!("{integer}"),
                Attribute::Range(decimal_1, decimal_2) =>
                    format!("{{{decimal_1},{decimal_2}}}"),
            }
        )
    }
}

impl From<&str> for Attribute {
    fn from(s: &str) -> Self {
        s.to_string().into()
    }
}

impl From<String> for Attribute {
    fn from(s: String) -> Self {
        if let Ok(integer) = s.parse() {
            Attribute::Integer(integer)
        } else if let Ok(decimal) = s.parse() {
            Attribute::Decimal(decimal)
        } else if let Ok(Attribute::Range(a, b)) = s.parse() {
            Attribute::Range(a, b)
        } else {
            Attribute::Text(s)
        }
    }
}

impl FromStr for Attribute {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with('{') && s.ends_with('}') {
            let split_s = s[1..s.len() - 1].split(',');
            let r: Vec<&str> = split_s.collect();
            if r.len() == 2 {
                if let Ok(a) = r.first().unwrap().parse() {
                    if let Ok(b) = r.last().unwrap().parse() {
                        Ok(Attribute::Range(a, b))
                    } else {
                        Ok(Attribute::Text(s.to_owned()))
                    }
                } else {
                    Ok(Attribute::Text(s.to_owned()))
                }
            } else {
                Ok(Attribute::Text(s.to_owned()))
            }
        } else {
            Ok(Attribute::Text(s.to_owned()))
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
pub enum NodeType {
    #[default]
    Unset,
    Tip,
    Internal,
    FirstNode,
    Root,
}

/// Represents a node in a phylogenetic tree.
///
/// ## Terminology
///
/// - **label**: The identifier/name for this node (e.g., taxon name for tips)
/// - **branch**: The edge connecting this node to its parent
/// - **branch_length**: The length of the branch leading to this node
/// - **node_attributes**: Metadata about the node itself (e.g., node age, taxon info)
/// - **branch_attributes**: Metadata about the branch leading to this node (e.g., support values)
#[derive(Debug, Default, Clone, PartialEq)]
pub struct Node {
    node_id: Option<NodeId>,
    parent_id: Option<NodeId>,
    child_ids: Vec<NodeId>,
    branch_length: Option<TreeFloat>,
    node_label: Option<Arc<str>>,
    node_attributes: HashMap<String, Attribute>,
    branch_attributes: HashMap<String, Attribute>,
    node_type: NodeType,
    edge_index: Option<usize>,
}

impl Node {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn node_attributes(&self) -> HashMap<String, Attribute> {
        self.node_attributes.clone()
    }

    pub fn branch_attributes(&self) -> HashMap<String, Attribute> {
        self.branch_attributes.clone()
    }

    pub fn set_node_attributes(
        &mut self,
        node_attributes: HashMap<String, Attribute>,
    ) {
        self.node_attributes = node_attributes;
    }

    pub fn set_branch_attributes(
        &mut self,
        branch_attributes: HashMap<String, Attribute>,
    ) {
        self.branch_attributes = branch_attributes;
    }

    pub fn branch_length(&self) -> Option<TreeFloat> {
        self.branch_length
    }

    pub fn set_branch_length(&mut self, branch_length: Option<TreeFloat>) {
        self.branch_length = branch_length;
    }

    pub fn node_label(&self) -> Option<Arc<str>> {
        self.node_label.clone()
    }

    pub fn set_node_label<'a>(&mut self, name: Option<impl Into<&'a str>>) {
        self.node_label = name.map(|name| name.into().into());
    }

    pub fn is_tip(&self) -> bool {
        self.node_type == NodeType::Tip
    }

    pub fn child_ids(&self) -> &[NodeId] {
        &self.child_ids
    }

    pub(super) fn set_child_ids(&mut self, child_ids: Vec<NodeId>) {
        self.child_ids = child_ids;
    }

    pub fn child_node_count(&self) -> usize {
        self.child_ids.len()
    }

    pub fn add_child_id(&mut self, node_id: NodeId) {
        self.child_ids.push(node_id);
    }

    pub fn add_child_ids<'a>(&mut self, node_ids: impl Into<&'a [NodeId]>) {
        self.child_ids.extend(node_ids.into());
    }

    pub fn remove_child_id(&mut self, node_id: &NodeId) {
        let idx = self.child_ids.iter().position(|id| id == node_id);
        if let Some(idx) = idx {
            _ = self.child_ids.swap_remove(idx);
        }
    }

    pub fn remove_child_ids<'a>(&mut self, node_ids: impl Into<&'a [NodeId]>) {
        for node_id in node_ids.into() {
            self.remove_child_id(node_id);
        }
    }

    pub(super) fn edge_index(&self) -> Option<usize> {
        self.edge_index
    }

    pub(super) fn set_edge_index(&mut self, edge_index: usize) {
        self.edge_index = Some(edge_index);
    }

    pub fn node_id(&self) -> Option<&NodeId> {
        self.node_id.as_ref()
    }

    pub fn set_node_id(&mut self, node_id: NodeId) {
        self.node_id = Some(node_id);
    }

    pub fn parent_id(&self) -> Option<&NodeId> {
        self.parent_id.as_ref()
    }

    pub fn set_parent_id(&mut self, node_id: Option<NodeId>) {
        self.parent_id = node_id;
    }

    pub fn node_type(&self) -> NodeType {
        self.node_type
    }

    pub fn set_node_type(&mut self) -> NodeType {
        if self.child_ids.len() >= 2 && self.parent_id.is_some() {
            self.node_type = NodeType::Internal;
        } else if self.child_ids.is_empty() && self.parent_id.is_some() {
            self.node_type = NodeType::Tip;
        } else if self.child_ids.len() >= 3 && self.parent_id.is_none() {
            self.node_type = NodeType::FirstNode;
        } else if self.child_ids.len() == 2 && self.parent_id.is_none() {
            self.node_type = NodeType::Root;
        } else if self.child_ids.len() <= 1 && self.parent_id.is_none() {
            self.node_type = NodeType::FirstNode;
        }
        self.node_type
    }
}

impl From<String> for Node {
    fn from(value: String) -> Self {
        let mut node = Node::default();
        let name = match value.as_str() {
            "" => None,
            v => Some(v),
        };
        node.set_node_label(name);
        node
    }
}

impl<'a> From<&'a str> for Node {
    fn from(value: &'a str) -> Self {
        let mut node = Node::default();
        let name = match value {
            "" => None,
            v => Some(v),
        };
        node.set_node_label(name);
        node
    }
}

impl Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let disp = format!("{self:?}");
        write!(f, "{}", &disp[7..disp.len() - 1])
    }
}

impl From<NodeId> for String {
    fn from(node_id: NodeId) -> Self {
        format!("{node_id}")
    }
}

impl Display for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                NodeType::Unset => "Unset",
                NodeType::Tip => "Tip",
                NodeType::Internal => "Internal",
                NodeType::Root => "Root",
                NodeType::FirstNode => "FirstNode",
            }
        )
    }
}
