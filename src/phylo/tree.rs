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
    #[error("Cannot use this node as outgroup: {0}.")]
    InvalidOutgroupNode(NodeId),
}

impl Tree {
    pub fn has_tip_labels(&self) -> bool {
        for n in self.nodes.values() {
            if n.is_tip() && n.name().is_some() {
                return true;
            }
        }
        false
    }

    pub fn has_int_labels(&self) -> bool {
        for n in self.nodes.values() {
            if !n.is_tip() && n.name().is_some() {
                return true;
            }
        }
        false
    }

    pub fn tip_heights(&self) -> Vec<(NodeId, TreeFloat)> {
        let mut heights: Vec<(NodeId, TreeFloat)> = Vec::new();
        if let Some(first_node_id) = self.first_node_id {
            for n in self.nodes.values() {
                if n.is_tip() {
                    if let Some(&node_id) = n.node_id() {
                        heights.push((node_id, self.dist(first_node_id, node_id)));
                    }
                }
            }
        }
        heights
    }

    pub fn is_ultrametric(&self, epsilon: TreeFloat) -> Option<bool> {
        if !self.is_rooted() {
            return None;
        }
        let is_ultrametric = true;
        let tip_heights = self.tip_heights();
        let mut prev_tip_height = tip_heights[0].1;
        for (_node_id, h) in tip_heights {
            if (prev_tip_height - h).abs() < epsilon {
                prev_tip_height = h;
            } else {
                return Some(false);
            }
        }
        Some(is_ultrametric)
    }

    pub fn can_root(&self, node_id: NodeId) -> bool {
        if let Some(first_node_id) = self.first_node_id {
            if node_id == first_node_id {
                return false;
            }
            if self.is_rooted() {
                let bad_outgroups = self.child_ids(first_node_id);
                if bad_outgroups.contains(&node_id) {
                    return false;
                }
            }
        }
        true
    }

    pub fn root(&mut self, node_id: NodeId) -> Result<NodeId, TreeError> {
        if !self.can_root(node_id) {
            return Err(TreeError::InvalidOutgroupNode(node_id));
        }
        let yanked_node = self.unroot();
        if let Some(left_id) = self.first_node_id {
            let new_root_id = self
                .add_new_node(<Option<&str>>::None, None, None)
                .ok()
                .unwrap();

            let path = self.path(node_id, left_id);
            let brl_new_out: TreeFloat = self.branch_length(node_id).unwrap_or_default() / 2e0;
            // println!("{node_id}:{brl_new_out} <- new out");
            if self.has_branch_lengths {
                self.nodes[node_id].set_branch_length(Some(brl_new_out));
            }
            self.nodes[node_id].set_parent_id(Some(new_root_id));

            // println!("---");
            // println!("{new_root_id} <- new root");
            self.nodes[new_root_id].add_child_id(node_id);
            // println!("---");

            let mut prev_brl = brl_new_out;
            let mut prev_par = new_root_id;
            for &id in &path {
                // println!("{id}:{prev_brl} {prev_par} <- to reverse");
                let brl_tmp = self.branch_length(id).unwrap_or_default();
                if self.has_branch_lengths {
                    self.nodes[id].set_branch_length(Some(prev_brl));
                }
                self.nodes[prev_par].add_child_id(id);
                self.nodes[id].set_parent_id(Some(prev_par));
                self.nodes[id].remove_child_id(&prev_par);
                self.nodes[id].remove_child_id(&node_id);
                prev_brl = brl_tmp;
                prev_par = id;
            }

            let mut new_node_name: Option<String> = None;
            if let Some(yanked_node) = yanked_node {
                if let Some(name) = yanked_node.name() {
                    new_node_name = Some(name.to_string());
                }
            }

            // println!("NEW LAST:{prev_brl} {prev_par} <- new last");
            let new_last = self
                .add_new_node(new_node_name.as_deref(), None, Some(prev_par))
                .ok()
                .unwrap();

            if self.has_branch_lengths {
                self.nodes[new_last].set_branch_length(Some(prev_brl));
            }

            let id_to_ignore: NodeId =
                if !path.is_empty() { path[path.len() - 1] } else { node_id };

            let tmp_chld_ids = self.child_ids(left_id).to_vec();
            for id in tmp_chld_ids {
                if id != id_to_ignore {
                    // let brl_last = self.branch_length(id).unwrap_or_default();
                    // println!("{id}:{brl_last:<5.3} <- child of last");
                    self.nodes[new_last].add_child_id(id);
                    self.nodes[id].set_parent_id(Some(new_last));
                }
            }
            self.nodes.remove(left_id);
        }
        self.validate()
    }

    pub fn path(&self, right_id: NodeId, left_id: NodeId) -> Vec<NodeId> {
        let mut rv: Vec<NodeId> = Vec::new();
        if left_id == right_id {
            return rv;
        }

        if let Some(&parent_id) = self.parent_id(right_id) {
            if parent_id != left_id {
                rv.push(parent_id);
                rv.extend(self.path(parent_id, left_id));
            }
        } else {
            return rv;
        }

        rv
    }

    pub fn node_id_by_name<'a>(&self, name: impl Into<&'a str>) -> Option<NodeId> {
        let name: &str = name.into();
        self.nodes.iter().find_map(|(node_id, node)| {
            if let Some(node_name) = node.name() {
                if node_name == name.into() { Some(node_id) } else { None }
            } else {
                None
            }
        })
    }

    pub fn unroot(&mut self) -> Option<Node> {
        let mut yanked_node: Option<Node> = None;
        if let Some(node_to_drop_id) = self.unroot_pick_node_to_drop() {
            self.slide_brlen_through_root(node_to_drop_id);
            yanked_node = self.node(Some(node_to_drop_id)).cloned();
            self.yank_node(node_to_drop_id);
            let _ = self.validate();
        }
        yanked_node
    }

    fn yank_node(&mut self, node_id: NodeId) {
        let child_ids = self.child_ids(node_id).to_vec();
        if let Some(parent_node_id) = self.nodes[node_id].parent_id() {
            let parent_node_id: NodeId = *parent_node_id;
            let parent = self.nodes.get_mut(parent_node_id).unwrap();

            parent.remove_child_id(&node_id);

            for &child_id in &child_ids {
                parent.add_child_id(child_id);
            }

            for &child_id in &child_ids {
                let child = self.nodes.get_mut(child_id).unwrap();
                child.set_parent_id(Some(parent_node_id));
            }

            self.nodes.remove(node_id);
        }
    }

    fn slide_brlen_through_root(&mut self, source_node_id: NodeId) {
        if !self.is_rooted() || !self.has_branch_lengths {
            return;
        }

        let mut receive_node_id: NodeId = source_node_id;
        for &other_id in self.child_ids(self.first_node_id.unwrap()) {
            if other_id != source_node_id {
                receive_node_id = other_id
            }
        }

        assert_ne!(source_node_id, receive_node_id);

        let source_node = &self.nodes[source_node_id];
        let receive_node = &self.nodes[receive_node_id];

        let mut new_brlen: TreeFloat = 0e0;
        if let Some(source_brlen) = source_node.branch_length() {
            if let Some(receive_brlen) = receive_node.branch_length() {
                new_brlen = receive_brlen + source_brlen
            } else {
                new_brlen = source_brlen
            }
        }

        self.nodes[source_node_id].set_branch_length(Some(0e0));
        self.nodes[receive_node_id].set_branch_length(Some(new_brlen));
    }

    fn unroot_pick_node_to_drop(&self) -> Option<NodeId> {
        if !self.is_rooted() {
            return None;
        }
        if let Some(root_node) = self.node(self.first_node_id) {
            if root_node.child_node_count() != 2 {
                return None;
            }
        }
        let root_chld = self.children(self.first_node_id.unwrap());
        let c1 = root_chld[0];
        let c2 = root_chld[1];

        if c1.is_tip() && c2.is_tip() {
            return None;
        }

        // At this point we know that at most one of c1 and c2 could still be a tip.
        let node_to_drop;
        if c1.is_tip() || c2.is_tip() {
            if c1.is_tip() {
                // c1 is a tip
                node_to_drop = c2;
            } else {
                // c2 must be a tip a this point
                node_to_drop = c1;
            }
        } else {
            // The only possible state at this point is that neither c1 or c2 are tips.
            // Arbitrarily drop the one with more total descending tips.
            let tcr1 = self.tip_count_recursive(*c1.node_id().unwrap());
            let tcr2 = self.tip_count_recursive(*c2.node_id().unwrap());

            if tcr1 > tcr2 {
                node_to_drop = c1;
            } else {
                node_to_drop = c2;
            }
        }

        node_to_drop.node_id().copied()
    }

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
        if let Some(id) = self.first_node_id { self.tip_node_ids(id) } else { Vec::new() }
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
            if !self.is_tip(child_id) { rv += self.tip_count_recursive(child_id) } else { rv += 1 }
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
        if let Some(node_id) = node_id { self.nodes.get(node_id) } else { None }
    }

    pub fn node_mut(&mut self, node_id: Option<NodeId>) -> Option<&mut Node> {
        if let Some(node_id) = node_id { self.nodes.get_mut(node_id) } else { None }
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

    pub fn validate(&mut self) -> Result<NodeId, TreeError> {
        // let mut count_of_unset: usize = 0;
        let mut count_of_tip: usize = 0;
        let mut count_of_internal: usize = 0;
        let mut count_of_first: usize = 0;
        let mut count_of_root: usize = 0;

        let mut has_branch_lengths: bool = false;

        // let mut unset_node_ids: Vec<NodeId> = Vec::new();
        for node in self.nodes.values_mut() {
            match node.set_node_type() {
                NodeType::Unset => {
                    // unset_node_ids.push(*node.node_id().unwrap());
                    // count_of_unset += 1
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

        // for node_id in unset_node_ids {
        //     let node = self.node(Some(node_id));
        //     let children = self.children(node_id);
        //     println!("-- PARENT:\n{node:#?}");
        //     println!("-- CHILDREN:\n{children:#?}")
        // }

        if count_of_first + count_of_root != 1 {
            return Err(TreeError::InvalidTree(format!(
                "count_of_first({count_of_first}) + count_of_root({count_of_root}) should equal 1."
            )));
        }

        // if count_of_unset != 0 {
        //     return Err(TreeError::InvalidTree(format!(
        //         "count_of_unset({count_of_unset}) should equal 0."
        //     )));
        // }

        self.tip_count_all = count_of_tip;
        self.internal_node_count_all = count_of_internal + count_of_first + count_of_root;
        self.node_count_all = self.tip_count_all + self.internal_node_count_all;

        self.has_branch_lengths = has_branch_lengths;

        if let Some(node) = self.node_mut(self.first_node_id) {
            node.set_branch_length(None);
        }

        Ok(self.first_node_id.unwrap())
    }

    fn print_tree(&self) -> String {
        let mut rv: String = String::new();
        rv.push_str(&format!(
            "Internal Nodes: {}\nTips: {}\nAll Nodes: {}\n{}\nHeight: {:7.5}\nBranch lengths: {}\n\n",
            self.internal_node_count_all,
            self.tip_count_all,
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
            "{}- {} | {} | {:<5.3} | {}\n",
            " ".repeat(_level * 4),
            if let Some(node_id) = node.node_id() {
                node_id.to_string()
            } else {
                "None".to_string()
            },
            if let Some(name) = &node.name() { name.to_string() } else { "None".to_string() },
            if let Some(brlen) = node.branch_length() { brlen } else { TreeFloat::NAN },
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
