use super::TreeFloat;
use slotmap::new_key_type;
use std::{fmt::Display, sync::Arc};

new_key_type! { pub struct NodeId; }

#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
pub enum NodeType {
    #[default]
    Unset,
    Tip,
    Internal,
    FirstNode,
    Root,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Node {
    node_id: Option<NodeId>,
    parent_id: Option<NodeId>,
    child_ids: Vec<NodeId>,
    branch_length: Option<TreeFloat>,
    node_label: Option<Arc<str>>,
    node_props: Vec<String>,
    branch_props: Vec<String>,
    node_type: NodeType,
    edge_idx: Option<usize>,
}

impl Node {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn node_props(&self) -> Vec<String> {
        self.node_props.clone()
    }

    pub fn set_node_props(&mut self, node_props: Vec<String>) {
        self.node_props = node_props;
    }

    pub fn branch_props(&self) -> Vec<String> {
        self.branch_props.clone()
    }

    pub fn set_branch_props(&mut self, branch_props: Vec<String>) {
        self.branch_props = branch_props;
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

    pub(super) fn edge_idx(&self) -> Option<usize> {
        self.edge_idx
    }

    pub(super) fn set_edge_idx(&mut self, edge_idx: usize) {
        self.edge_idx = Some(edge_idx);
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
