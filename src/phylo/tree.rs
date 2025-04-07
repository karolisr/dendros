use super::TreeFloat;
use super::node::{Node, NodeId, NodeType};
use slotmap::SlotMap;
use std::fmt::Display;
use std::sync::Arc;
use thiserror::Error;

#[derive(Debug, Default, Clone)]
pub struct Tree {
    nodes: SlotMap<NodeId, Node>,
    first_node_id: Option<NodeId>,
    tip_count_all: usize,
    internal_node_count_all: usize,
    node_count_all: usize,
}

#[derive(Debug, Error)]
pub enum TreeError {
    #[error("Parent node with NodeId: {0} does not exist.")]
    ParentNodeDoesNotExist(NodeId),
    #[error("Tree validation failed: {0}.")]
    InvalidTree(String),
}

impl Tree {
    pub fn unroot(&self) {
        unimplemented!()
    }

    pub fn height(&self) -> TreeFloat {
        unimplemented!()
    }

    pub fn sort(&mut self, reverse: bool) {
        unimplemented!()
    }

    pub fn branch_length(&self, node_id: NodeId) -> Option<TreeFloat> {
        self.nodes[node_id].branch_length()
    }

    pub fn tip_count_all(&self) -> usize {
        self.tip_count_all
    }

    pub fn internal_node_count_all(&self) -> usize {
        self.internal_node_count_all
    }

    pub fn node_count_all(&self) -> usize {
        self.node_count_all
    }

    pub fn tip_count(&self, node_id: NodeId) -> usize {
        let mut rv: usize = 0;
        if self.nodes[node_id].is_tip() {
            rv = 1;
        } else {
            for child_node in self.children(node_id) {
                if child_node.is_tip() {
                    rv += 1
                }
            }
        }
        rv
    }

    pub fn tip_count_recursive(&self, node_id: NodeId) -> usize {
        unimplemented!()
    }

    pub fn new() -> Self {
        Self::default()
    }

    pub fn name(&self, node_id: NodeId) -> Option<Arc<str>> {
        self.nodes[node_id].name()
    }

    pub fn child_ids(&self, node_id: NodeId) -> &[NodeId] {
        self.nodes[node_id].child_ids()
    }

    pub fn children(&self, node_id: NodeId) -> Vec<&Node> {
        let mut rv = Vec::new();
        for &child_id in self.child_ids(node_id) {
            let child_node = &self.nodes[child_id];
            rv.push(child_node);
        }
        rv
    }

    pub fn add_new_node<'a>(
        &mut self,
        name: Option<impl Into<&'a str>>,
        branch_length: Option<TreeFloat>,
        parent_node_id: Option<NodeId>,
    ) -> Result<NodeId, TreeError> {
        let mut node: Node = Node::default();
        node.set_name(name);
        node.set_branch_length(branch_length);
        self.add_node(node, parent_node_id)
    }

    pub fn add_node(
        &mut self,
        node: Node,
        parent_node_id: Option<NodeId>,
    ) -> Result<NodeId, TreeError> {
        let nodes: Vec<Node> = vec![node];
        let rslt = self.add_nodes(nodes, parent_node_id);
        match rslt {
            Ok(node_ids) => Ok(node_ids[0]),
            Err(err) => Err(err),
        }
    }

    pub fn add_nodes(
        &mut self,
        nodes: impl Into<Vec<Node>>,
        parent_node_id: Option<NodeId>,
    ) -> Result<Vec<NodeId>, TreeError> {
        let mut nodes: Vec<Node> = nodes.into().to_vec();

        if parent_node_id.is_some() {
            if self.node_exists(parent_node_id) {
                for node in &mut nodes {
                    node.set_parent_id(parent_node_id);
                }
            } else {
                return Err(TreeError::ParentNodeDoesNotExist(parent_node_id.unwrap()));
            }
        }

        let mut node_ids: Vec<NodeId> = Vec::new();

        for mut node in nodes {
            let node_id = self.nodes.insert_with_key(|node_id| {
                node.set_node_id(node_id);
                node
            });

            node_ids.push(node_id);

            if let Some(parent_node) = self.node_mut(parent_node_id) {
                parent_node.add_child_id(node_id);
            }
        }

        Ok(node_ids)
    }

    pub fn node(&self, node_id: Option<NodeId>) -> Option<&Node> {
        if let Some(node_id) = node_id {
            self.nodes.get(node_id)
        } else {
            None
        }
    }

    pub fn node_mut(&mut self, node_id: Option<NodeId>) -> Option<&mut Node> {
        if let Some(node_id) = node_id {
            self.nodes.get_mut(node_id)
        } else {
            None
        }
    }

    pub fn node_exists(&self, node_id: Option<NodeId>) -> bool {
        self.node(node_id).is_some()
    }

    pub fn first_node_id(&self) -> Option<NodeId> {
        self.first_node_id
    }

    pub fn is_rooted(&self) -> bool {
        if let Some(node) = self.node(self.first_node_id()) {
            return node.node_type() == NodeType::Root;
        }
        false
    }

    pub fn validate(&mut self) -> Result<(), TreeError> {
        let mut count_of_unset: usize = 0;
        let mut count_of_tip: usize = 0;
        let mut count_of_internal: usize = 0;
        let mut count_of_first: usize = 0;
        let mut count_of_root: usize = 0;

        for node in self.nodes.values_mut() {
            match node.set_node_type() {
                NodeType::Unset => count_of_unset += 1,
                NodeType::Tip => count_of_tip += 1,
                NodeType::Internal => count_of_internal += 1,
                NodeType::FirstNode => {
                    count_of_first += 1;
                    if let Some(node_id) = node.node_id() {
                        self.first_node_id = Some(*node_id)
                    }
                }
                NodeType::Root => {
                    count_of_root += 1;
                    if let Some(node_id) = node.node_id() {
                        self.first_node_id = Some(*node_id)
                    }
                }
            };
        }

        if count_of_first + count_of_root != 1 {
            return Err(TreeError::InvalidTree(format!(
                "count_of_first({count_of_first}) + count_of_root({count_of_root}) should equal 1."
            )));
        }

        if count_of_unset != 0 {
            return Err(TreeError::InvalidTree(format!(
                "count_of_unset({count_of_unset}) should equal 0."
            )));
        }

        self.tip_count_all = count_of_tip;
        self.internal_node_count_all = count_of_internal + count_of_first + count_of_root;
        self.node_count_all = self.tip_count_all + self.internal_node_count_all;

        if let Some(node) = self.node_mut(self.first_node_id) {
            node.set_branch_length(None);
        }

        Ok(())
    }

    fn print_tree(&self) -> String {
        let mut rv: String = String::new();
        rv.push_str(&format!(
            "Tips: {}\nInternal Nodes: {}\nAll Nodes: {}\n{}\n\n",
            self.tip_count_all,
            self.internal_node_count_all,
            self.node_count_all,
            match self.is_rooted() {
                true => "Rooted",
                false => "Unrooted",
            }
        ));

        if let Some(node) = self.node(self.first_node_id) {
            rv.push_str(&self.print_node(node, 0));
        }

        rv
    }

    fn print_node(&self, node: &Node, _level: usize) -> String {
        let mut rv: String = String::new();
        rv.push_str(&format!(
            "{}- {} | {} | {} | {}\n",
            " ".repeat(_level * 4),
            if let Some(node_id) = node.node_id() {
                node_id.to_string()
            } else {
                "None".to_string()
            },
            if let Some(name) = &node.name() {
                name.to_string()
            } else {
                "None".to_string()
            },
            if let Some(brlen) = node.branch_length() {
                brlen
            } else {
                TreeFloat::NAN
            },
            node.node_type()
        ));

        for &id in node.child_ids() {
            rv.push_str(&self.print_node(&self.nodes[id], _level + 1));
        }

        rv
    }
}

impl Display for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print_tree())
    }
}
