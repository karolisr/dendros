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
    has_branch_lengths: bool,
}

#[derive(Debug, Error)]
pub enum TreeError {
    #[error("Parent node with NodeId: {0} does not exist.")]
    ParentNodeDoesNotExist(NodeId),
    #[error("Tree validation failed: {0}.")]
    InvalidTree(String),
}

impl Tree {
    pub fn unroot(&self) {}

    pub fn has_branch_lengths(&self) -> bool {
        self.has_branch_lengths
    }

    pub fn is_tip(&self, node_id: NodeId) -> bool {
        self.nodes[node_id].is_tip()
    }

    pub fn tip_node_ids(&self, node_id: NodeId) -> Vec<NodeId> {
        let cs: &[NodeId] = self.child_ids(node_id);
        let mut rv: Vec<NodeId> = Vec::new();
        for &c in cs {
            if self.is_tip(c) {
                rv.push(c);
            } else {
                rv.append(&mut self.tip_node_ids(c));
            }
        }
        rv
    }

    pub fn tip_node_ids_all(&self) -> Vec<NodeId> {
        if let Some(id) = self.first_node_id {
            self.tip_node_ids(id)
        } else {
            Vec::new()
        }
    }

    pub fn height(&self) -> TreeFloat {
        let mut h = 0e0;
        if let Some(id) = self.first_node_id {
            for right in self.tip_node_ids_all() {
                let curr = self.dist(id, right);
                if curr > h {
                    h = curr
                }
            }
        }
        h
    }

    pub fn dist(&self, left: NodeId, right: NodeId) -> TreeFloat {
        let mut h: TreeFloat = 0e0;
        if left != right {
            h += self.branch_length(right).unwrap_or(0e0);
        }
        match self.parent_id(right) {
            Some(&p) => {
                if p == left {
                    h
                } else {
                    h + self.dist(left, p)
                }
            }
            None => 0e0,
        }
    }

    pub fn child_count(&self, node_id: NodeId) -> usize {
        self.nodes[node_id].child_node_count()
    }

    pub fn child_count_recursive(&self, node_id: NodeId) -> usize {
        let mut rv: usize = self.child_count(node_id);
        for &child_id in self.child_ids(node_id) {
            rv += self.child_count_recursive(child_id)
        }
        rv
    }

    pub fn sort(&mut self, reverse: bool) {
        if let Some(id) = self.first_node_id {
            self.sort_nodes(id, reverse);
        }
    }

    fn sort_nodes(&mut self, node_id: NodeId, reverse: bool) {
        let mut sorted_ids: Vec<NodeId> = self.nodes[node_id].child_ids().to_vec();
        sorted_ids.sort_by_key(|c| self.child_count_recursive(*c));
        if reverse {
            sorted_ids.reverse();
        }
        for &id in &sorted_ids {
            self.sort_nodes(id, reverse);
        }
        self.nodes[node_id].set_child_ids(sorted_ids);
    }

    pub fn tip_count_recursive(&self, node_id: NodeId) -> usize {
        let mut rv: usize = 0;
        for &child_id in self.child_ids(node_id) {
            if !self.is_tip(child_id) {
                rv += self.tip_count_recursive(child_id)
            } else {
                rv += 1
            }
        }
        rv
    }

    pub fn tip_count(&self, node_id: NodeId) -> usize {
        let mut rv: usize = 0;
        for child_node in self.children(node_id) {
            if child_node.is_tip() {
                rv += 1
            }
        }
        rv
    }

    pub fn new() -> Self {
        Self::default()
    }

    pub fn branch_length(&self, node_id: NodeId) -> Option<TreeFloat> {
        if self.has_branch_lengths {
            self.nodes[node_id].branch_length()
        } else if Some(node_id) == self.first_node_id {
            None
        } else {
            Some(1e0)
        }
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

    pub fn name(&self, node_id: NodeId) -> Option<Arc<str>> {
        self.nodes[node_id].name()
    }

    pub fn parent_id(&self, node_id: NodeId) -> Option<&NodeId> {
        self.nodes[node_id].parent_id()
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

        let mut has_branch_lengths: bool = false;

        let mut unset_node_ids: Vec<NodeId> = Vec::new();
        for node in self.nodes.values_mut() {
            match node.set_node_type() {
                NodeType::Unset => {
                    unset_node_ids.push(*node.node_id().unwrap());
                    count_of_unset += 1
                }
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

            if node.node_type() != NodeType::FirstNode && node.node_type() != NodeType::Root {
                match node.branch_length() {
                    Some(_) => has_branch_lengths = true,
                    None => has_branch_lengths = false,
                }
            }
        }

        for node_id in unset_node_ids {
            let node = self.node(Some(node_id));
            let children = self.children(node_id);
            println!("-- PARENT:\n{node:#?}");
            println!("-- CHILDREN:\n{children:#?}")
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

        self.has_branch_lengths = has_branch_lengths;

        if let Some(node) = self.node_mut(self.first_node_id) {
            node.set_branch_length(None);
        }

        Ok(())
    }

    fn print_tree(&self) -> String {
        let mut rv: String = String::new();
        rv.push_str(&format!(
            "Tips: {}\nInternal Nodes: {}\nAll Nodes: {}\n{}\nHeight: {:7.5}\nBranch lengths: {}\n\n",
            self.tip_count_all,
            self.internal_node_count_all,
            self.node_count_all,
            match self.is_rooted() {
                true => "Rooted",
                false => "Unrooted",
            },
            self.height(),
            self.has_branch_lengths()
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
