use super::Edge;
use super::TreeFloat;
use super::attribute::{Attribute, AttributeType};
use super::flatten_tree;
use super::node::{Node, NodeId, NodeType};
use rayon::prelude::*;
use rustc_hash::FxHashSet;
use slotmap::SlotMap;
use std::collections::HashMap;
use std::fmt::Display;
use std::sync::Arc;
use thiserror::Error;

#[derive(Debug, Default, Clone)]
pub struct Tree {
    nodes: SlotMap<NodeId, Node>,
    edges: Option<Vec<Edge>>,
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
    #[error("Attribute key '{0}' already exists and cannot be renamed to.")]
    AttributeKeyAlreadyExists(String),
    #[error("Attribute '{0}' not found on node {1}.")]
    AttributeNotFound(String, NodeId),
    #[error("Attribute type mismatch: cannot convert {0} to {1}.")]
    AttributeTypeMismatch(String, String),
}

impl Tree {
    // =========================================================================
    // Construction & Validation
    // =========================================================================

    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_new_node<'a>(
        &mut self,
        name: Option<impl Into<&'a str>>,
        branch_length: Option<TreeFloat>,
        parent_node_id: Option<NodeId>,
    ) -> Result<NodeId, TreeError> {
        let mut node: Node = Node::default();
        node.set_node_label(name);
        node.set_branch_length(branch_length);
        self.add_node(node, parent_node_id)
    }

    pub fn add_node(
        &mut self,
        node: Node,
        parent_node_id: Option<NodeId>,
    ) -> Result<NodeId, TreeError> {
        let nodes: Vec<Node> = vec![node];
        let result = self.add_nodes(nodes, parent_node_id);
        match result {
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

        if let Some(parent_node_id_value) = parent_node_id {
            if self.node_exists(parent_node_id) {
                for node in &mut nodes {
                    node.set_parent_id(parent_node_id);
                    self.edges = None;
                }
            } else {
                return Err(TreeError::ParentNodeDoesNotExist(
                    parent_node_id_value,
                ));
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

    pub fn validate(
        &mut self,
        make_fresh_edges: bool,
    ) -> Result<NodeId, TreeError> {
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
                        self.first_node_id = Some(*node_id);
                    }
                }
                NodeType::Root => {
                    count_of_root += 1;
                    if let Some(node_id) = node.node_id() {
                        self.first_node_id = Some(*node_id);
                    }
                }
            };

            if node.node_type() != NodeType::FirstNode
                && node.node_type() != NodeType::Root
            {
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
        self.internal_node_count_all =
            count_of_internal + count_of_first + count_of_root;
        self.node_count_all = self.tip_count_all + self.internal_node_count_all;

        self.has_branch_lengths = has_branch_lengths;

        if let Some(node) = self.node_mut(self.first_node_id) {
            node.set_branch_length(None);
        }

        if make_fresh_edges {
            self.rebuild_edges();
        }

        // Validate attribute type consistency.
        self.validate_and_unify_attribute_types()?;

        Ok(self.first_node_id.unwrap())
    }

    /// Validates and unifies attribute types across all nodes to ensure consistency.
    fn validate_and_unify_attribute_types(&mut self) -> Result<(), TreeError> {
        use std::collections::HashMap;

        let mut node_attribute_types: HashMap<String, Vec<AttributeType>> =
            HashMap::new();

        let mut branch_attribute_types: HashMap<String, Vec<AttributeType>> =
            HashMap::new();

        // First pass: collect all attribute types.
        for node in self.nodes.values() {
            let node_attrs = node.node_attributes();
            let branch_attrs = node.branch_attributes();

            for (key, attr) in node_attrs {
                node_attribute_types
                    .entry(key)
                    .or_default()
                    .push(attr.get_type());
            }

            for (key, attr) in branch_attrs {
                branch_attribute_types
                    .entry(key)
                    .or_default()
                    .push(attr.get_type());
            }
        }

        // Second pass: determine unified types for each attribute key.
        let mut unified_node_types: HashMap<String, AttributeType> =
            HashMap::new();

        let mut unified_branch_types: HashMap<String, AttributeType> =
            HashMap::new();

        for (key, types) in node_attribute_types {
            let unified_type =
                Self::unify_attribute_types(&types, &key, "node")?;
            let _ = unified_node_types.insert(key, unified_type);
        }

        for (key, types) in branch_attribute_types {
            let unified_type =
                Self::unify_attribute_types(&types, &key, "branch")?;
            let _ = unified_branch_types.insert(key, unified_type);
        }

        // Third pass: convert attributes to unified types.
        for node in self.nodes.values_mut() {
            let mut updated_node_attrs = HashMap::new();
            let mut updated_branch_attrs = HashMap::new();

            for (key, attr) in node.node_attributes() {
                if let Some(target_type) = unified_node_types.get(&key) {
                    let unified_attr =
                        Self::convert_attribute_to_type(attr, target_type)?;
                    let _ = updated_node_attrs.insert(key, unified_attr);
                } else {
                    let _ = updated_node_attrs.insert(key, attr);
                }
            }

            for (key, attr) in node.branch_attributes() {
                if let Some(target_type) = unified_branch_types.get(&key) {
                    let unified_attr =
                        Self::convert_attribute_to_type(attr, target_type)?;
                    let _ = updated_branch_attrs.insert(key, unified_attr);
                } else {
                    let _ = updated_branch_attrs.insert(key, attr);
                }
            }

            node.set_node_attributes(updated_node_attrs);
            node.set_branch_attributes(updated_branch_attrs);
        }

        Ok(())
    }

    /// Gets the unified type for a specific attribute key across all nodes.
    fn get_unified_type_for_key(
        &self,
        key: &str,
        is_branch: bool,
    ) -> Option<AttributeType> {
        let mut attribute_types: Vec<AttributeType> = Vec::new();

        for node in self.nodes.values() {
            let attrs = if is_branch {
                node.branch_attributes()
            } else {
                node.node_attributes()
            };

            if let Some(attr) = attrs.get(key) {
                attribute_types.push(attr.get_type());
            }
        }

        if attribute_types.is_empty() {
            return None;
        }

        let mut unified_type = attribute_types[0].clone();
        for current_type in attribute_types.iter().skip(1) {
            if let Some(new_unified) =
                Attribute::get_unified_type(&unified_type, current_type)
            {
                unified_type = new_unified;
            } else {
                return None;
            }
        }

        Some(unified_type)
    }

    /// Unifies a list of attribute types into a single consistent type.
    fn unify_attribute_types(
        types: &[AttributeType],
        key: &str,
        attr_kind: &str,
    ) -> Result<AttributeType, TreeError> {
        if types.is_empty() {
            return Err(TreeError::InvalidTree(format!(
                "No types found for {} attribute '{}'",
                attr_kind, key
            )));
        }

        let mut unified_type = types[0].clone();

        for (i, current_type) in types.iter().enumerate().skip(1) {
            if let Some(new_unified) =
                Attribute::get_unified_type(&unified_type, current_type)
            {
                unified_type = new_unified;
            } else {
                return Err(TreeError::InvalidTree(format!(
                    "Incompatible types for {} attribute '{}': cannot unify {:?} (from node {}) with {:?}",
                    attr_kind, key, unified_type, i, current_type
                )));
            }
        }

        Ok(unified_type)
    }

    /// Converts an attribute to match a target type.
    fn convert_attribute_to_type(
        attr: Attribute,
        target_type: &AttributeType,
    ) -> Result<Attribute, TreeError> {
        let current_type = attr.get_type();

        // If types already match, return as-is.
        if current_type == *target_type {
            return Ok(attr);
        }

        // Integer --> Decimal conversion.
        match (current_type, target_type) {
            (AttributeType::Integer, AttributeType::Decimal) => {
                Ok(attr.unify_to_decimal())
            }
            (
                AttributeType::List(current_items),
                AttributeType::List(target_items),
            ) => {
                // Convert list items, if needed.
                if current_items.len() != target_items.len() {
                    return Err(TreeError::InvalidTree(format!(
                        "List length mismatch: expected {} items, got {}",
                        target_items.len(),
                        current_items.len()
                    )));
                }

                Ok(attr.unify_to_decimal()) // Integer -> Decimal conversion in lists.
            }
            _ => Ok(attr), // No conversion needed OR already handled.
        }
    }

    // =========================================================================
    // Tree Properties
    // =========================================================================

    pub fn has_tip_labels(&self) -> bool {
        for n in self.nodes.values() {
            if n.is_tip() && n.node_label().is_some() {
                return true;
            }
        }
        false
    }

    pub fn has_internal_node_labels(&self) -> bool {
        for n in self.nodes.values() {
            if !n.is_tip() && n.node_label().is_some() {
                return true;
            }
        }
        false
    }

    pub fn tip_heights(&self) -> Vec<(NodeId, TreeFloat)> {
        if let Some(first_node_id) = self.first_node_id {
            let tip_nodes = self.tip_node_ids_all();
            let mut heights = Vec::with_capacity(tip_nodes.len());

            // Use parallel processing for larger trees
            if tip_nodes.len() > 100 {
                heights = tip_nodes
                    .par_iter()
                    .map(|&node_id| {
                        (node_id, self.distance(&first_node_id, &node_id))
                    })
                    .collect();
            } else {
                for &node_id in &tip_nodes {
                    heights.push((
                        node_id,
                        self.distance(&first_node_id, &node_id),
                    ));
                }
            }

            heights
        } else {
            Vec::new()
        }
    }

    pub fn is_ultrametric(&self, epsilon: TreeFloat) -> Option<bool> {
        if !self.is_rooted() {
            return None;
        }

        let tip_heights = self.tip_heights();
        if tip_heights.is_empty() {
            return Some(true);
        }

        let first_height = tip_heights[0].1;
        for (_, height) in tip_heights.iter().skip(1) {
            if (first_height - height).abs() >= epsilon {
                return Some(false);
            }
        }

        Some(true)
    }

    pub fn has_branch_lengths(&self) -> bool {
        self.has_branch_lengths
    }

    pub fn height(&self) -> TreeFloat {
        if let Some(id) = &self.first_node_id {
            let tip_ids = self.tip_node_ids_all();

            // Use parallel processing for larger trees
            if tip_ids.len() > 100 {
                tip_ids
                    .par_iter()
                    .map(|right| self.distance(id, right))
                    .reduce(|| 0.0, f64::max)
            } else {
                tip_ids
                    .iter()
                    .map(|right| self.distance(id, right))
                    .fold(0.0, f64::max)
            }
        } else {
            0.0
        }
    }

    pub fn distance(
        &self,
        left_node_id: &NodeId,
        right_node_id: &NodeId,
    ) -> TreeFloat {
        if left_node_id == right_node_id {
            return 0.0;
        }

        // For now, keep the original implementation as memoization would require &mut self
        // TODO: Consider using RefCell for interior mutability if memoization is needed
        let mut total_distance: TreeFloat = 0.0;
        total_distance += self.branch_length(*right_node_id).unwrap_or(0.0);

        if let Some(parent_node_id) = self.parent_id(right_node_id) {
            if parent_node_id != left_node_id {
                total_distance += self.distance(left_node_id, parent_node_id);
            }
        }

        total_distance
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

    // =========================================================================
    // Node Access
    // =========================================================================

    pub fn node(&self, node_id: Option<NodeId>) -> Option<&Node> {
        if let Some(node_id) = node_id { self.nodes.get(node_id) } else { None }
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

    pub fn node_ids_all(&self) -> Vec<NodeId> {
        self.nodes.keys().collect()
    }

    pub fn node_id_by_label<'a>(
        &self,
        label: impl Into<&'a str>,
    ) -> Option<NodeId> {
        let label: &str = label.into();
        self.nodes.iter().find_map(|(node_id, node)| {
            if let Some(node_label) = node.node_label() {
                if node_label == label.into() { Some(node_id) } else { None }
            } else {
                None
            }
        })
    }

    pub fn label(&self, node_id: &NodeId) -> Option<Arc<str>> {
        self.nodes[*node_id].node_label()
    }

    // =========================================================================
    // Tree Traversal
    // =========================================================================

    pub fn path(
        &self,
        right_node_id: &NodeId,
        left_node_id: &NodeId,
    ) -> Vec<NodeId> {
        if left_node_id == right_node_id {
            return Vec::new();
        }

        let mut path = Vec::new();
        let mut current_node_id = right_node_id;

        while let Some(parent_node_id) = self.parent_id(current_node_id) {
            if parent_node_id == left_node_id {
                break;
            }
            path.push(*parent_node_id);
            current_node_id = parent_node_id;
        }

        path
    }

    pub fn parent_id(&self, node_id: &NodeId) -> Option<&NodeId> {
        self.nodes[*node_id].parent_id()
    }

    pub fn child_ids(&self, node_id: &NodeId) -> &[NodeId] {
        self.nodes[*node_id].child_ids()
    }

    pub fn first_child_id(&self, node_id: &NodeId) -> Option<&NodeId> {
        self.child_ids(node_id).first()
    }

    pub fn last_child_id(&self, node_id: &NodeId) -> Option<&NodeId> {
        self.child_ids(node_id).last()
    }

    pub fn children(&self, node_id: &NodeId) -> Vec<&Node> {
        let mut result = Vec::new();
        for &child_id in self.child_ids(node_id) {
            let child_node = &self.nodes[child_id];
            result.push(child_node);
        }
        result
    }

    pub fn child_count(&self, node_id: &NodeId) -> usize {
        self.nodes[*node_id].child_node_count()
    }

    pub fn child_count_recursive(&self, node_id: &NodeId) -> usize {
        let children = self.child_ids(node_id);
        children.len()
            + children
                .iter()
                .map(|child_id| self.child_count_recursive(child_id))
                .sum::<usize>()
    }

    pub fn is_tip(&self, node_id: &NodeId) -> bool {
        self.nodes[*node_id].is_tip()
    }

    pub fn tip_node_ids(&self, node_id: &NodeId) -> Vec<NodeId> {
        let mut result = Vec::new();
        self.collect_tip_ids_recursive(*node_id, &mut result);
        result
    }

    fn collect_tip_ids_recursive(
        &self,
        node_id: NodeId,
        result: &mut Vec<NodeId>,
    ) {
        if self.is_tip(&node_id) {
            result.push(node_id);
        } else {
            for child_id in self.child_ids(&node_id) {
                self.collect_tip_ids_recursive(*child_id, result);
            }
        }
    }

    pub fn tip_node_ids_all(&self) -> Vec<NodeId> {
        if let Some(id) = self.first_node_id {
            self.tip_node_ids(&id)
        } else {
            Vec::new()
        }
    }

    pub fn tip_count_recursive(&self, node_id: &NodeId) -> usize {
        let mut count: usize = 0;
        for child_id in self.child_ids(node_id) {
            if !self.is_tip(child_id) {
                count += self.tip_count_recursive(child_id);
            } else {
                count += 1;
            }
        }
        count
    }

    pub fn tip_count(&self, node_id: &NodeId) -> usize {
        let mut count: usize = 0;
        for child_node in self.children(node_id) {
            if child_node.is_tip() {
                count += 1;
            }
        }
        count
    }

    pub fn bounding_tip_ids_for_clade<'a>(
        &'a self,
        node_id: &'a NodeId,
    ) -> (&'a NodeId, &'a NodeId) {
        (
            self.first_tip_id_for_clade(node_id),
            self.last_tip_id_for_clade(node_id),
        )
    }

    pub fn first_tip_id_for_clade<'a>(
        &'a self,
        node_id: &'a NodeId,
    ) -> &'a NodeId {
        match self.first_child_id(node_id) {
            Some(child_id) => self.first_tip_id_for_clade(child_id),
            None => node_id,
        }
    }

    pub fn last_tip_id_for_clade<'a>(
        &'a self,
        node_id: &'a NodeId,
    ) -> &'a NodeId {
        match self.last_child_id(node_id) {
            Some(child_id) => self.last_tip_id_for_clade(child_id),
            None => node_id,
        }
    }

    // =========================================================================
    // Rooting Operations
    // =========================================================================

    pub fn is_rooted(&self) -> bool {
        if let Some(node) = self.node(self.first_node_id()) {
            return node.node_type() == NodeType::Root;
        }
        false
    }

    pub fn is_valid_potential_outgroup_node(&self, node_id: &NodeId) -> bool {
        if let Some(first_node_id) = &self.first_node_id {
            if node_id == first_node_id {
                return false;
            }
            if self.is_rooted() {
                let bad_outgroups = self.child_ids(first_node_id);
                if bad_outgroups.contains(node_id) {
                    return false;
                }
            }
        }
        true
    }

    pub fn root(&mut self, node_id: NodeId) -> Result<NodeId, TreeError> {
        if !self.is_valid_potential_outgroup_node(&node_id) {
            return Err(TreeError::InvalidOutgroupNode(node_id));
        }

        let yanked_node = self.unroot();

        if let Some(left_node_id) = self.first_node_id {
            let new_root_id = self
                .add_new_node(<Option<&str>>::None, None, None)
                .ok()
                .unwrap();

            let path = self.path(&node_id, &left_node_id);

            let branch_length_new_outgroup: TreeFloat =
                self.branch_length(node_id).unwrap_or_default() / 2e0;

            let branch_attributes_new_outgroup =
                self.branch_attributes(node_id);

            if self.has_branch_lengths {
                self.nodes[node_id]
                    .set_branch_length(Some(branch_length_new_outgroup));
            }

            self.nodes[node_id].set_parent_id(Some(new_root_id));
            self.nodes[new_root_id].add_child_id(node_id);

            let mut previous_branch_length = branch_length_new_outgroup;
            let mut previous_branch_attributes = branch_attributes_new_outgroup;
            let mut previous_parent_node_id = new_root_id;

            for &path_node_id in &path {
                let temporary_branch_length =
                    self.branch_length(path_node_id).unwrap_or_default();

                let temporary_branch_attributes =
                    self.branch_attributes(path_node_id);

                if self.has_branch_lengths {
                    self.nodes[path_node_id]
                        .set_branch_length(Some(previous_branch_length));
                }

                self.nodes[path_node_id]
                    .set_branch_attributes(previous_branch_attributes);

                self.nodes[previous_parent_node_id].add_child_id(path_node_id);

                self.nodes[path_node_id]
                    .set_parent_id(Some(previous_parent_node_id));

                self.nodes[path_node_id]
                    .remove_child_id(&previous_parent_node_id);

                self.nodes[path_node_id].remove_child_id(&node_id);

                previous_branch_attributes = temporary_branch_attributes;
                previous_branch_length = temporary_branch_length;
                previous_parent_node_id = path_node_id;
            }

            let mut new_node_label: Option<String> = self.nodes[left_node_id]
                .node_label()
                .map(|node_label| node_label.to_string());

            if let Some(yanked_node) = yanked_node
                && let Some(label) = yanked_node.node_label()
            {
                new_node_label = Some(label.to_string());
            }

            let new_last = self
                .add_new_node(
                    new_node_label.as_deref(),
                    None,
                    Some(previous_parent_node_id),
                )
                .ok()
                .unwrap();

            if self.has_branch_lengths {
                self.nodes[new_last]
                    .set_branch_length(Some(previous_branch_length));
            }

            self.nodes[new_last]
                .set_branch_attributes(previous_branch_attributes);

            let node_id_to_ignore: NodeId =
                if !path.is_empty() { path[path.len() - 1] } else { node_id };

            let temporary_child_ids = self.child_ids(&left_node_id).to_vec();

            for child_node_id in temporary_child_ids {
                if child_node_id != node_id_to_ignore {
                    self.nodes[new_last].add_child_id(child_node_id);
                    self.nodes[child_node_id].set_parent_id(Some(new_last));
                }
            }
            _ = self.nodes.remove(left_node_id);
        }

        self.edges = None;
        self.validate(true)
    }

    pub fn unroot(&mut self) -> Option<Node> {
        let mut yanked_node: Option<Node> = None;
        if let Some(node_to_drop_id) = self.select_root_child_node_to_drop() {
            self.slide_brlen_through_root(node_to_drop_id);
            yanked_node = self.node(Some(node_to_drop_id)).cloned();
            self.yank_node(node_to_drop_id);
            self.edges = None;
            let _ = self.validate(true);
        }
        yanked_node
    }

    fn yank_node(&mut self, node_id: NodeId) {
        let child_ids = self.child_ids(&node_id).to_vec();
        if let Some(parent_node_id) = self.nodes[node_id].parent_id() {
            let parent_node_id: NodeId = *parent_node_id;
            let parent_node = self.nodes.get_mut(parent_node_id).unwrap();

            parent_node.remove_child_id(&node_id);

            for &child_id in &child_ids {
                parent_node.add_child_id(child_id);
            }

            for &child_id in &child_ids {
                let child_node = self.nodes.get_mut(child_id).unwrap();
                child_node.set_parent_id(Some(parent_node_id));
            }

            _ = self.nodes.remove(node_id);
        }
    }

    fn slide_brlen_through_root(&mut self, source_node_id: NodeId) {
        if !self.is_rooted() || !self.has_branch_lengths {
            return;
        }

        let mut receive_node_id: NodeId = source_node_id;
        for &other_id in self.child_ids(&self.first_node_id.unwrap()) {
            if other_id != source_node_id {
                receive_node_id = other_id;
            }
        }

        assert_ne!(source_node_id, receive_node_id);

        let source_node = &self.nodes[source_node_id];
        let receive_node = &self.nodes[receive_node_id];

        let mut new_brlen: TreeFloat = 0e0;
        if let Some(source_brlen) = source_node.branch_length() {
            if let Some(receive_brlen) = receive_node.branch_length() {
                new_brlen = receive_brlen + source_brlen;
            } else {
                new_brlen = source_brlen;
            }
        }

        self.nodes[source_node_id].set_branch_length(Some(0e0));
        self.nodes[receive_node_id].set_branch_length(Some(new_brlen));
    }

    fn select_root_child_node_to_drop(&self) -> Option<NodeId> {
        if !self.is_rooted() {
            return None;
        }
        if let Some(root_node) = self.node(self.first_node_id)
            && root_node.child_node_count() != 2
        {
            return None;
        }
        let root_child_nodes = self.children(&self.first_node_id.unwrap());
        let child_node_1 = root_child_nodes[0];
        let child_node_2 = root_child_nodes[1];

        if child_node_1.is_tip() && child_node_2.is_tip() {
            return None;
        }

        // At this point we know that at most one of child_node_1 and
        // child_node_2 could still be a tip.
        let node_to_drop;
        if child_node_1.is_tip() || child_node_2.is_tip() {
            if child_node_1.is_tip() {
                // child_node_1 is a tip.
                node_to_drop = child_node_2;
            } else {
                // child_node_2 must be a tip a this point.
                node_to_drop = child_node_1;
            }
        } else {
            // The only possible state at this point is that neither
            // child_node_1 or child_node_2 are tips. Arbitrarily drop the one
            // with more total descending tips.
            let tip_count_1 =
                self.tip_count_recursive(child_node_1.node_id().unwrap());
            let tip_count_2 =
                self.tip_count_recursive(child_node_2.node_id().unwrap());

            if tip_count_1 > tip_count_2 {
                node_to_drop = child_node_1;
            } else {
                node_to_drop = child_node_2;
            }
        }

        node_to_drop.node_id().copied()
    }

    // =========================================================================
    // Edge Operations
    // =========================================================================

    pub fn needs_edge_rebuild(&self) -> bool {
        self.edges.is_none()
    }

    pub fn rebuild_edges(&mut self) {
        if let Some(_) = self.first_node_id
            && self.needs_edge_rebuild()
        {
            let mut edges = flatten_tree(self);

            for (edge_index, edge) in edges.iter_mut().enumerate() {
                edge.edge_index = edge_index;
                self.nodes[edge.node_id].set_edge_index(edge_index);
            }

            self.edges = Some(edges);
        }
    }

    pub fn edges(&self) -> Option<&Vec<Edge>> {
        self.edges.as_ref()
    }

    pub fn edge_index_for_node_id(&self, node_id: NodeId) -> Option<usize> {
        self.node(Some(node_id))?.edge_index()
    }

    pub fn bounding_tip_edges_for_clade(
        &self,
        node_id: &NodeId,
    ) -> Option<(&Edge, &Edge)> {
        if !self.node_exists(Some(*node_id)) {
            return None;
        }

        let edges = self.edges()?;
        let (&tip_node_id_0, &tip_node_id_1) =
            self.bounding_tip_ids_for_clade(node_id);
        let start: &Edge =
            &edges[self.edge_index_for_node_id(tip_node_id_0)?];
        let end: &Edge = &edges[self.edge_index_for_node_id(tip_node_id_1)?];

        Some((start, end))
    }

    pub fn bounding_edges_for_clade(
        &self,
        node_id: &NodeId,
    ) -> Option<(Vec<Edge>, Vec<Edge>)> {
        if !self.node_exists(Some(*node_id)) {
            return None;
        }

        let edges = self.edges()?;

        let (top_tip_node_id, bottom_tip_node_id) =
            self.bounding_tip_ids_for_clade(node_id);

        let mut edges_top: Vec<Edge> = Vec::new();
        let mut edges_bottom: Vec<Edge> = Vec::new();

        let path_top = self.path(top_tip_node_id, node_id);
        let path_bottom = self.path(bottom_tip_node_id, node_id);

        edges_top.push(
            edges[self.edge_index_for_node_id(*top_tip_node_id)?].clone(),
        );

        for path_node_id in path_top {
            let edge = &edges[self.edge_index_for_node_id(path_node_id)?];
            edges_top.push(edge.clone());
        }

        for &path_node_id in path_bottom.iter().rev() {
            let edge = &edges[self.edge_index_for_node_id(path_node_id)?];
            edges_bottom.push(edge.clone());
        }

        edges_bottom.push(
            edges[self.edge_index_for_node_id(*bottom_tip_node_id)?].clone(),
        );

        Some((edges_top, edges_bottom))
    }

    // =========================================================================
    // Tree Manipulation
    // =========================================================================

    pub fn sort(&mut self, reverse: bool) {
        if let Some(node_id) = self.first_node_id {
            self.sort_nodes(node_id, reverse);
            self.edges = None;
            self.rebuild_edges();
        }
    }

    pub fn sorted(tree: &Self, reverse: bool) -> Self {
        let mut temporary_tree = tree.clone();
        temporary_tree.sort(reverse);
        temporary_tree
    }

    fn sort_nodes(&mut self, node_id: NodeId, reverse: bool) {
        let mut sorted_ids: Vec<NodeId> =
            self.nodes[node_id].child_ids().to_vec();
        sorted_ids.par_sort_by_key(|c| self.child_count_recursive(c));
        if reverse {
            sorted_ids.reverse();
        }
        for &child_node_id in &sorted_ids {
            self.sort_nodes(child_node_id, reverse);
        }
        self.nodes[node_id].set_child_ids(sorted_ids);
    }

    // =========================================================================
    // Attributes
    // =========================================================================

    pub fn branch_attributes(
        &self,
        node_id: NodeId,
    ) -> HashMap<String, Attribute> {
        self.nodes[node_id].branch_attributes()
    }

    pub fn node_attributes(
        &self,
        node_id: NodeId,
    ) -> HashMap<String, Attribute> {
        self.nodes[node_id].node_attributes()
    }

    pub fn branch_attribute_keys(&self) -> Vec<String> {
        let mut result: FxHashSet<String> = FxHashSet::default();
        for node in self.nodes.values() {
            let attribute_keys: FxHashSet<String> = FxHashSet::from_iter(
                self.branch_attributes(*node.node_id().unwrap())
                    .keys()
                    .cloned(),
            );
            result.extend(attribute_keys);
        }
        result.into_iter().collect()
    }

    pub fn node_attribute_keys(&self) -> Vec<String> {
        let mut result: FxHashSet<String> = FxHashSet::default();
        for node in self.nodes.values() {
            let attribute_keys: FxHashSet<String> = FxHashSet::from_iter(
                self.node_attributes(*node.node_id().unwrap()).keys().cloned(),
            );
            result.extend(attribute_keys);
        }
        result.into_iter().collect()
    }

    pub fn branch_length(&self, node_id: NodeId) -> Option<TreeFloat> {
        if self.has_branch_lengths {
            self.nodes[node_id].branch_length()
        } else if Some(node_id) == self.first_node_id() {
            None
        } else {
            Some(1e0)
        }
    }

    // =========================================================================
    // Attribute Management
    // =========================================================================

    /// Rename a node attribute key across all nodes that have it.
    ///
    /// Arguments:
    /// * `old_key` - The current attribute key name
    /// * `new_key` - The new attribute key name
    ///
    /// Returns:
    /// * `Ok(count)` - Number of nodes that had the attribute renamed
    /// * `Err(TreeError::AttributeKeyAlreadyExists)` - If new_key already exists
    pub fn rename_node_attribute_key(
        &mut self,
        old_key: &str,
        new_key: &str,
    ) -> Result<usize, TreeError> {
        // Check if new_key already exists on any node
        for node in self.nodes.values() {
            if node.node_attributes().contains_key(new_key) {
                return Err(TreeError::AttributeKeyAlreadyExists(
                    new_key.to_string(),
                ));
            }
        }

        let mut count = 0;
        for node in self.nodes.values_mut() {
            let mut node_attrs = node.node_attributes();
            if let Some(attr) = node_attrs.remove(old_key) {
                let _ = node_attrs.insert(new_key.to_string(), attr);
                node.set_node_attributes(node_attrs);
                count += 1;
            }
        }

        Ok(count)
    }

    /// Rename a branch attribute key across all nodes that have it.
    ///
    /// Arguments:
    /// * `old_key` - The current attribute key name
    /// * `new_key` - The new attribute key name
    ///
    /// Returns:
    /// * `Ok(count)` - Number of nodes that had the attribute renamed
    /// * `Err(TreeError::AttributeKeyAlreadyExists)` - If new_key already exists
    pub fn rename_branch_attribute_key(
        &mut self,
        old_key: &str,
        new_key: &str,
    ) -> Result<usize, TreeError> {
        // Check if new_key already exists on any branch
        for node in self.nodes.values() {
            if node.branch_attributes().contains_key(new_key) {
                return Err(TreeError::AttributeKeyAlreadyExists(
                    new_key.to_string(),
                ));
            }
        }

        let mut count = 0;
        for node in self.nodes.values_mut() {
            let mut branch_attrs = node.branch_attributes();
            if let Some(attr) = branch_attrs.remove(old_key) {
                let _ = branch_attrs.insert(new_key.to_string(), attr);
                node.set_branch_attributes(branch_attrs);
                count += 1;
            }
        }

        Ok(count)
    }

    /// Change the value of a node attribute for a specific node.
    ///
    /// Arguments:
    /// * `node_id` - The node to update
    /// * `key` - The attribute key to change
    /// * `new_value` - The new attribute value
    ///
    /// Returns:
    /// * `Ok(())` - Success
    /// * `Err(TreeError::ParentNodeDoesNotExist)` - If node doesn't exist
    /// * `Err(TreeError::AttributeNotFound)` - If attribute doesn't exist on node
    /// * `Err(TreeError::AttributeTypeMismatch)` - If new value type is incompatible
    pub fn change_node_attribute_value(
        &mut self,
        node_id: NodeId,
        key: &str,
        new_value: Attribute,
    ) -> Result<(), TreeError> {
        if !self.node_exists(Some(node_id)) {
            return Err(TreeError::ParentNodeDoesNotExist(node_id));
        }

        // Check if the attribute exists on the node
        if !self
            .node(Some(node_id))
            .unwrap()
            .node_attributes()
            .contains_key(key)
        {
            return Err(TreeError::AttributeNotFound(key.to_string(), node_id));
        }

        // Get the unified type for this attribute key across all nodes.
        let unified_type = self.get_unified_type_for_key(key, false);

        // Get mutable reference to the node.
        let node = self.node_mut(Some(node_id)).unwrap();

        if let Some(unified_type) = unified_type {
            // Validate that new_value can be converted to the unified type.
            let new_value_type = new_value.get_type();
            if !Attribute::can_unify_types(&new_value_type, &unified_type) {
                return Err(TreeError::AttributeTypeMismatch(
                    format!("{:?}", new_value_type),
                    format!("{:?}", unified_type),
                ));
            }

            // Convert new_value if needed.
            let converted_value = if new_value_type != unified_type {
                Self::convert_attribute_to_type(new_value, &unified_type)?
            } else {
                new_value
            };

            // Update the node's attribute.
            let mut node_attrs = node.node_attributes();
            let _ = node_attrs.insert(key.to_string(), converted_value);
            node.set_node_attributes(node_attrs);
        } else {
            // If no unified type exists, just update the attribute
            let mut node_attrs = node.node_attributes();
            let _ = node_attrs.insert(key.to_string(), new_value);
            node.set_node_attributes(node_attrs);
        }

        Ok(())
    }

    /// Change the value of a branch attribute for a specific node.
    ///
    /// Arguments:
    /// * `node_id` - The node to update
    /// * `key` - The attribute key to change
    /// * `new_value` - The new attribute value
    ///
    /// Returns:
    /// * `Ok(())` - Success
    /// * `Err(TreeError::ParentNodeDoesNotExist)` - If node doesn't exist
    /// * `Err(TreeError::AttributeNotFound)` - If attribute doesn't exist on node
    /// * `Err(TreeError::AttributeTypeMismatch)` - If new value type is incompatible
    pub fn change_branch_attribute_value(
        &mut self,
        node_id: NodeId,
        key: &str,
        new_value: Attribute,
    ) -> Result<(), TreeError> {
        if !self.node_exists(Some(node_id)) {
            return Err(TreeError::ParentNodeDoesNotExist(node_id));
        }

        // Check if the attribute exists on the node
        if !self
            .node(Some(node_id))
            .unwrap()
            .branch_attributes()
            .contains_key(key)
        {
            return Err(TreeError::AttributeNotFound(key.to_string(), node_id));
        }

        // Get the unified type for this attribute key across all nodes
        let unified_type = self.get_unified_type_for_key(key, true);

        // Get mutable reference to the node
        let node = self.node_mut(Some(node_id)).unwrap();

        if let Some(unified_type) = unified_type {
            // Validate that new_value can be converted to the unified type
            let new_value_type = new_value.get_type();
            if !Attribute::can_unify_types(&new_value_type, &unified_type) {
                return Err(TreeError::AttributeTypeMismatch(
                    format!("{:?}", new_value_type),
                    format!("{:?}", unified_type),
                ));
            }

            // Convert new_value if needed
            let converted_value = if new_value_type != unified_type {
                Self::convert_attribute_to_type(new_value, &unified_type)?
            } else {
                new_value
            };

            // Update the node's attribute
            let mut branch_attrs = node.branch_attributes();
            let _ = branch_attrs.insert(key.to_string(), converted_value);
            node.set_branch_attributes(branch_attrs);
        } else {
            // If no unified type exists, just update the attribute
            let mut branch_attrs = node.branch_attributes();
            let _ = branch_attrs.insert(key.to_string(), new_value);
            node.set_branch_attributes(branch_attrs);
        }

        Ok(())
    }

    // =========================================================================
    // Display
    // =========================================================================

    fn print_tree(&self) -> String {
        let mut result: String = String::new();
        result.push_str(&format!(
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
            result.push_str(&self.print_node(node, 0));
        }

        result
    }

    fn print_node(&self, node: &Node, _level: usize) -> String {
        let mut result: String = String::new();
        let branch_attributes_text: String = node
            .branch_attributes()
            .iter()
            .map(|(key, value)| format!("{key}: {value}; "))
            .collect();
        let node_attributes_text: String = node
            .node_attributes()
            .iter()
            .map(|(key, value)| format!("{key}: {value}; "))
            .collect();
        result.push_str(&format!(
            "{}- {} | {} | {:<5.3} | {} | {}| {}\n",
            " ".repeat(_level * 4),
            if let Some(node_id) = node.node_id() {
                node_id.to_string()
            } else {
                "None".to_string()
            },
            if let Some(label) = &node.node_label() {
                label.to_string()
            } else {
                "None".to_string()
            },
            if let Some(branch_length) = node.branch_length() {
                branch_length
            } else {
                TreeFloat::NAN
            },
            node.node_type(),
            branch_attributes_text,
            node_attributes_text,
        ));

        for &child_node_id in node.child_ids() {
            result.push_str(
                &self.print_node(&self.nodes[child_node_id], _level + 1),
            );
        }

        result
    }
}

impl Display for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print_tree())
    }
}
