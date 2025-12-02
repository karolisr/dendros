use super::attribute::Attribute;
use super::attribute::AttributeType;
use super::edges::Edge;
use super::edges::prepare_edges;
use super::node::Node;
use super::node::NodeId;
use super::node::NodeType;
use crate::IndexRange;
use crate::TreeFloat;

use rayon::prelude::*;
use slotmap::SlotMap;
use thiserror::Error;

use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FormatterResult;
use std::sync::Arc;

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
    #[error("Node with NodeId: {0} does not exist.")]
    NodeDoesNotExist(NodeId),

    #[error("Node with NodeId: {0} does not have a parent.")]
    NodeDoesNotHaveParent(NodeId),

    #[error("Could not get a mutable reference to node with NodeId: {0}.")]
    FailedToObtainMutableReferenceToNode(NodeId),

    #[error("Tree validation failed: {0}.")]
    InvalidTree(String),

    #[error("Cannot use this node as outgroup: {0}.")]
    InvalidOutgroupNode(NodeId),

    #[error("The tree is not rooted.")]
    UnrootedTree,

    #[error("Child node count for node {node_id}: {child_node_count} != 2")]
    ChildNodeCountIsNotTwo { node_id: NodeId, child_node_count: usize },

    #[error(
        "Both child nodes of node {parent_node_id} are tips: {child1_node_id}, {child2_node_id}."
    )]
    BothChildNodesAreTips {
        parent_node_id: NodeId,
        child1_node_id: NodeId,
        child2_node_id: NodeId,
    },

    #[error("Attribute key '{0}' already exists and cannot be renamed to.")]
    AttributeKeyAlreadyExists(String),

    #[error("Attribute '{attribute_key}' not found on node {node_id}.")]
    AttributeNotFound { attribute_key: String, node_id: NodeId },

    #[error(
        "Attribute type mismatch: cannot convert {attribute_type_from} to {attribute_type_to}."
    )]
    AttributeTypeMismatch {
        attribute_type_from: AttributeType,
        attribute_type_to: AttributeType,
    },
}

impl<'a> Tree {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn subtree(&self, subtree_node_id: NodeId) -> Result<Self, TreeError> {
        if !self.node_exists(Some(subtree_node_id)) {
            return Err(TreeError::NodeDoesNotExist(subtree_node_id));
        }

        let mut subtree = self.clone();

        subtree.edges = None;

        let mut subtree_node_ids: HashSet<NodeId> =
            HashSet::from_iter(subtree.descending_node_ids(subtree_node_id));

        _ = subtree_node_ids.insert(subtree_node_id);

        self.node_ids_all().iter().for_each(|node_id| {
            if !subtree_node_ids.contains(node_id) {
                _ = subtree.nodes.remove(*node_id);
            }
        });

        if let Some(node) = subtree.node_mut(Some(subtree_node_id)) {
            node.set_parent_id(None);
        }

        _ = subtree.validate()?;

        Ok(subtree)
    }

    pub fn add_new_node(
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

    pub(crate) fn add_node(
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

    pub(crate) fn add_nodes(
        &mut self,
        nodes: impl Into<Vec<Node>>,
        parent_node_id_opt: Option<NodeId>,
    ) -> Result<Vec<NodeId>, TreeError> {
        let mut nodes: Vec<Node> = nodes.into().to_vec();

        if let Some(parent_node_id) = parent_node_id_opt {
            if self.node_exists(parent_node_id_opt) {
                for node in &mut nodes {
                    node.set_parent_id(parent_node_id_opt);
                    self.edges = None;
                }
            } else {
                return Err(TreeError::NodeDoesNotExist(parent_node_id));
            }
        }

        let mut node_ids: Vec<NodeId> = Vec::new();

        for mut node in nodes {
            let node_id = self.nodes.insert_with_key(|node_id| {
                node.set_node_id(node_id);
                node
            });

            node_ids.push(node_id);

            if let Some(parent_node) = self.node_mut(parent_node_id_opt) {
                parent_node.add_child_id(node_id);
            }
        }

        Ok(node_ids)
    }

    /// Removes a "knuckle" node from the tree while preserving branch length.
    ///
    /// A "knuckle" node is an internal node with exactly one child, which
    /// represents an unnecessary degree-2 vertex in the tree topology. Such
    /// nodes can arise from tree editing operations and should be removed to
    /// maintain proper tree structure.
    ///
    /// **What constitutes a knuckle:**
    /// - An internal node (not a tip) with exactly one child
    /// - Creates unnecessary complexity in tree topology
    /// - Often results from algorithmic artifacts or manual tree editing
    ///
    /// **Removal process:**
    /// 1. **Branch length preservation**: Adds the knuckle node's branch length
    ///    to its child's branch length, preserving total distance
    /// 2. **Label consolidation**: Combines node labels using "\_KNUCKLE\_" as
    ///    separator:
    ///    - `knuckle_label + "_KNUCKLE_" + child_label` (both have labels)
    ///    - `knuckle_label + "_KNUCKLE"` (only knuckle has label)
    ///    - `"KNUCKLE_" + child_label` (only child has label)
    ///    - No label change (neither has labels)
    /// 3. **Topology update**: Uses `yank_node()` to remove the knuckle while
    ///    connecting its child directly to its parent
    ///
    /// **Example transformation:**
    /// ```text
    /// Before knuckle removal:    After knuckle removal:
    ///        A                         A
    ///        | bl=0.1                  | bl=0.1+0.2=0.3
    ///        B knuckle                 C
    ///        | bl=0.2                 /|\
    ///        C                       D E F
    ///       /|\
    ///      D E F
    /// ```
    ///
    /// **Usage context:**
    /// - Called during tree validation (`validate()`) for nodes with
    ///   `NodeType::Unset`
    /// - Automatically triggered when `knuckle_removal_pass=false` and any
    ///   nodes with type `NodeType::Unset` are detected
    ///
    /// **Debug output:**
    /// Prints removal information to stdout for debugging purposes, showing:
    /// - Node IDs and labels involved in the operation
    /// - Before/after state of affected nodes
    ///
    /// **Arguments:**
    /// - `knuckle_node_id` - The ID of the putative knuckle node to examine and
    ///   potentially remove
    ///
    /// **Behavior:**
    /// - If the node has exactly one child: removes the knuckle as described
    ///   above
    /// - If the node has zero or multiple children: no operation performed
    /// - Uses safe node access patterns, skipping operations if nodes cannot be
    ///   accessed
    ///
    /// **Note:**
    ///
    /// This function only handles knuckle nodes (degree-2 vertices). It does
    /// not validate or modify other aspects of tree topology.
    ///
    fn remove_knuckle(&mut self, knuckle_node_id: NodeId) {
        let putative_knuckle_child_node_ids =
            self.child_node_ids(knuckle_node_id);

        // Check if this node is a knuckle.
        if putative_knuckle_child_node_ids.len() == 1 {
            let knuckle_branch_length_opt = self.branch_length(knuckle_node_id);
            let knuckle_node_label_opt = self.label(knuckle_node_id);
            let child_node_id = putative_knuckle_child_node_ids[0];
            let child_branch_length_opt = self.branch_length(child_node_id);
            let child_node_label_opt = self.label(child_node_id);

            if let Some(child) = self.node_mut(Some(child_node_id)) {
                // Adds knuckle's branch length to the child's branch length.
                if let Some(knuckle_branch_length) = knuckle_branch_length_opt {
                    if let Some(child_branch_length) = child_branch_length_opt {
                        child.set_branch_length(Some(
                            knuckle_branch_length + child_branch_length,
                        ));
                    }
                }

                // Updates child's node label by prepending knuckle's node label.
                if let Some(knuckle_node_label) = knuckle_node_label_opt {
                    if let Some(child_node_label) = child_node_label_opt {
                        let new_node_label = format!(
                            "{}_KNUCKLE_{}",
                            knuckle_node_label, child_node_label
                        );
                        child.set_node_label(Some(new_node_label.as_str()));
                        #[cfg(debug_assertions)]
                        println!(
                            "Removing knuckle: {}({}) -> {}({})",
                            knuckle_node_id,
                            knuckle_node_label,
                            child_node_id,
                            new_node_label
                        );
                    } else {
                        let new_node_label =
                            format!("{}_KNUCKLE", knuckle_node_label);
                        child.set_node_label(Some(new_node_label.as_str()));
                        #[cfg(debug_assertions)]
                        println!(
                            "Removing knuckle: {}({}) -> {}({})",
                            knuckle_node_id,
                            knuckle_node_label,
                            child_node_id,
                            new_node_label
                        );
                    }
                } else if let Some(child_node_label) = child_node_label_opt {
                    let new_node_label =
                        format!("KNUCKLE_{}", child_node_label);
                    child.set_node_label(Some(new_node_label.as_str()));
                    #[cfg(debug_assertions)]
                    println!(
                        "Removing knuckle: {} -> {}({})",
                        knuckle_node_id, child_node_id, new_node_label
                    );
                } else {
                    #[cfg(debug_assertions)]
                    println!(
                        "Removing knuckle: {} -> {}",
                        knuckle_node_id, child_node_id
                    );
                }

                _ = self.yank_node(knuckle_node_id);
            }
        }
    }

    pub fn validate(&mut self) -> Result<NodeId, TreeError> {
        self.validate_internal(false)
    }

    pub(crate) fn validate_internal(
        &mut self,
        knuckle_removal_pass: bool,
    ) -> Result<NodeId, TreeError> {
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
                    unset_node_ids.push(node.node_id().unwrap());
                    count_of_unset += 1;
                }
                NodeType::Tip => count_of_tip += 1,
                NodeType::Internal => count_of_internal += 1,
                NodeType::FirstNode => {
                    count_of_first += 1;
                    if let Some(node_id) = node.node_id() {
                        self.first_node_id = Some(node_id);
                    }
                }
                NodeType::Root => {
                    count_of_root += 1;
                    if let Some(node_id) = node.node_id() {
                        self.first_node_id = Some(node_id);
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

        self.has_branch_lengths = has_branch_lengths;

        // Removes knuckles.
        if count_of_unset != 0 && !knuckle_removal_pass {
            for putative_knuckle_node_id in unset_node_ids {
                self.remove_knuckle(putative_knuckle_node_id);
            }
            return self.validate_internal(true);
        }

        // Count of "unset_node_ids" should be 0 after knuckle removal.
        // This "for" loop should never execute; if it ever does, I didn't
        // account for something.
        for node_id in unset_node_ids {
            let node = self.node(Some(node_id));
            let child_nodes = self.child_nodes(node_id);
            println!("------        Node:\n{node:#?}");
            println!("------ Child Nodes:\n{child_nodes:#?}");
        }

        if count_of_first + count_of_root != 1 {
            return Err(TreeError::InvalidTree(format!(
                "count_of_first({count_of_first}) + count_of_root({count_of_root}) should equal 1"
            )));
        }

        if count_of_unset != 0 {
            return Err(TreeError::InvalidTree(format!(
                "count_of_unset({count_of_unset}) should equal 0"
            )));
        }

        self.tip_count_all = count_of_tip;
        self.internal_node_count_all =
            count_of_internal + count_of_first + count_of_root;
        self.node_count_all = self.tip_count_all + self.internal_node_count_all;

        if let Some(node) = self.node_mut(self.first_node_id) {
            node.set_branch_length(None);
        }

        self.validate_and_unify_attribute_types()?;

        if let Some(first_node_id) = self.first_node_id() {
            Ok(first_node_id)
        } else {
            Err(TreeError::InvalidTree(
                "The value of first_node_id is None; This should never happen."
                    .to_string(),
            ))
        }
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
                    .entry(key.to_string())
                    .or_default()
                    .push(attr.get_type());
            }

            for (key, attr) in branch_attrs {
                branch_attribute_types
                    .entry(key.to_string())
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
            let mut updated_node_attrs: HashMap<String, Attribute> =
                HashMap::new();
            let mut updated_branch_attrs: HashMap<String, Attribute> =
                HashMap::new();

            for (key, attr) in node.node_attributes() {
                if let Some(target_type) = unified_node_types.get(key) {
                    let unified_attr = Self::convert_attribute_to_type(
                        attr.clone(),
                        target_type,
                    )?;
                    let _ = updated_node_attrs
                        .insert(key.to_string(), unified_attr);
                } else {
                    let _ = updated_node_attrs
                        .insert(key.to_string(), attr.clone());
                }
            }

            for (key, attr) in node.branch_attributes() {
                if let Some(target_type) = unified_branch_types.get(key) {
                    let unified_attr = Self::convert_attribute_to_type(
                        attr.clone(),
                        target_type,
                    )?;
                    let _ = updated_branch_attrs
                        .insert(key.to_string(), unified_attr);
                } else {
                    let _ = updated_branch_attrs
                        .insert(key.to_string(), attr.clone());
                }
            }

            node.set_node_attributes(updated_node_attrs);
            node.set_branch_attributes(updated_branch_attrs);
        }

        Ok(())
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

    fn node_tip_distances(&self, node_id: NodeId) -> Vec<(NodeId, TreeFloat)> {
        self.tip_node_ids_all()
            .par_iter()
            .map(|&tip_node_id| {
                (tip_node_id, self.distance(node_id, tip_node_id))
            })
            .collect()
    }

    pub fn is_ultrametric(&self, epsilon: TreeFloat) -> Option<bool> {
        if !self.is_rooted() {
            return None;
        }

        let root_tip_distances =
            self.node_tip_distances(self.first_node_id().unwrap());

        if root_tip_distances.is_empty() {
            return Some(true);
        }

        let first = root_tip_distances[0].1;
        for (_, current) in root_tip_distances.iter().skip(1) {
            if (first - current).abs() >= epsilon {
                return Some(false);
            }
        }

        Some(true)
    }

    pub fn has_branch_lengths(&self) -> bool {
        self.has_branch_lengths
    }

    pub fn max_first_node_to_tip_distance(&self) -> TreeFloat {
        self.max_node_to_tip_distance(self.first_node_id().unwrap())
    }

    pub fn max_node_to_tip_distance(&self, node_id: NodeId) -> TreeFloat {
        let tip_ids = self.tip_node_ids(node_id);
        tip_ids
            .par_iter()
            .map(|right| self.distance(node_id, *right))
            .reduce(|| 0.0, TreeFloat::max)
    }

    pub fn first_node_to_node_distance(&self, node_id: NodeId) -> TreeFloat {
        self.distance(self.first_node_id().unwrap(), node_id)
    }

    pub fn distance(
        &self,
        left_node_id: NodeId,
        right_node_id: NodeId,
    ) -> TreeFloat {
        if left_node_id == right_node_id {
            return 0.0;
        }

        let mut total_distance: TreeFloat = 0.0;
        total_distance += self.branch_length(right_node_id).unwrap_or(0.0);

        if let Some(parent_node_id) = self.parent_node_id(right_node_id) {
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

    pub fn node_id_by_label(
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

    pub fn label(&self, node_id: NodeId) -> Option<Arc<str>> {
        self.node(Some(node_id))?.node_label()
    }

    pub fn branch_length(&self, node_id: NodeId) -> Option<TreeFloat> {
        if self.has_branch_lengths {
            self.node(Some(node_id))?.branch_length()
        } else if Some(node_id) == self.first_node_id() {
            None
        } else {
            Some(1e0)
        }
    }

    pub fn path(
        &self,
        right_node_id: NodeId,
        left_node_id: NodeId,
    ) -> Vec<NodeId> {
        if left_node_id == right_node_id {
            return Vec::new();
        }

        let mut path = Vec::new();
        let mut current_node_id = right_node_id;

        while let Some(parent_node_id) = self.parent_node_id(current_node_id) {
            if parent_node_id == left_node_id {
                break;
            }
            path.push(parent_node_id);
            current_node_id = parent_node_id;
        }

        path
    }

    pub fn parent_node_id(&self, node_id: NodeId) -> Option<NodeId> {
        self.node(Some(node_id))?.parent_id()
    }

    pub fn child_node_ids(&self, node_id: NodeId) -> &[NodeId] {
        if let Some(node) = self.node(Some(node_id)) {
            node.child_ids()
        } else {
            &[]
        }
    }

    pub fn first_child_node_id(&self, node_id: NodeId) -> Option<&NodeId> {
        self.child_node_ids(node_id).first()
    }

    pub fn last_child_node_id(&self, node_id: NodeId) -> Option<&NodeId> {
        self.child_node_ids(node_id).last()
    }

    pub fn child_nodes(&self, node_id: NodeId) -> Vec<&Node> {
        self.child_node_ids(node_id)
            .iter()
            .map(|&child_id| &self.nodes[child_id])
            .collect()
    }

    pub fn child_node_count(&self, node_id: NodeId) -> usize {
        self.nodes[node_id].child_node_count()
    }

    pub fn child_node_count_recursive(&self, node_id: NodeId) -> usize {
        let children = self.child_node_ids(node_id);
        children.len()
            + children
                .iter()
                .map(|&child_id| self.child_node_count_recursive(child_id))
                .sum::<usize>()
    }

    pub fn descending_node_ids(&self, node_id: NodeId) -> Vec<NodeId> {
        let mut result = Vec::new();
        self.child_node_ids_recursive(node_id, &mut result);
        result
    }

    fn child_node_ids_recursive(
        &self,
        node_id: NodeId,
        result: &mut Vec<NodeId>,
    ) {
        result.push(node_id);
        for child_id in self.child_node_ids(node_id) {
            self.child_node_ids_recursive(*child_id, result);
        }
    }

    pub fn is_tip(&self, node_id: NodeId) -> bool {
        self.nodes[node_id].is_tip()
    }

    pub fn tip_node_ids(&self, node_id: NodeId) -> Vec<NodeId> {
        let mut result = Vec::new();
        self.tip_node_ids_recursive(node_id, &mut result);
        result
    }

    fn tip_node_ids_recursive(
        &self,
        node_id: NodeId,
        result: &mut Vec<NodeId>,
    ) {
        if self.is_tip(node_id) {
            result.push(node_id);
        } else {
            for child_id in self.child_node_ids(node_id) {
                self.tip_node_ids_recursive(*child_id, result);
            }
        }
    }

    pub fn tip_node_ids_all(&self) -> Vec<NodeId> {
        if let Some(first_node_id) = self.first_node_id {
            self.tip_node_ids(first_node_id)
        } else {
            Vec::new()
        }
    }

    pub fn tip_node_count_recursive(&self, node_id: NodeId) -> usize {
        let mut count: usize = 0;
        for &child_id in self.child_node_ids(node_id) {
            if !self.is_tip(child_id) {
                count += self.tip_node_count_recursive(child_id);
            } else {
                count += 1;
            }
        }
        count
    }

    pub fn tip_node_count(&self, node_id: NodeId) -> usize {
        let mut count: usize = 0;
        for child_node in self.child_nodes(node_id) {
            if child_node.is_tip() {
                count += 1;
            }
        }
        count
    }

    pub fn is_rooted(&self) -> bool {
        if let Some(node) = self.node(self.first_node_id()) {
            return node.node_type() == NodeType::Root;
        }
        false
    }

    pub fn is_valid_potential_outgroup_node(&self, node_id: NodeId) -> bool {
        if let Some(first_node_id) = self.first_node_id {
            if node_id == first_node_id {
                return false;
            }
            if self.is_rooted() {
                let bad_outgroups = self.child_node_ids(first_node_id);
                if bad_outgroups.contains(&node_id) {
                    return false;
                }
            }
        }
        true
    }

    pub fn root(&mut self, node_id: NodeId) -> Result<NodeId, TreeError> {
        if !self.is_valid_potential_outgroup_node(node_id) {
            return Err(TreeError::InvalidOutgroupNode(node_id));
        }

        // Only unroot if the tree is currently rooted
        let yanked_node_opt =
            if self.is_rooted() { Some(self.unroot()?) } else { None };

        if let Some(left_node_id) = self.first_node_id {
            let new_root_id = self
                .add_new_node(<Option<&str>>::None, None, None)
                .ok()
                .unwrap();

            let path = self.path(node_id, left_node_id);

            let branch_length_new_outgroup: TreeFloat =
                self.branch_length(node_id).unwrap_or_default() / 2e0;

            let branch_attributes_new_outgroup =
                self.branch_attributes(node_id).clone();

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
                    self.branch_attributes(path_node_id).clone();

                if self.has_branch_lengths {
                    self.nodes[path_node_id]
                        .set_branch_length(Some(previous_branch_length));
                }

                self.nodes[path_node_id]
                    .set_branch_attributes(previous_branch_attributes.clone());

                self.nodes[previous_parent_node_id].add_child_id(path_node_id);

                self.nodes[path_node_id]
                    .set_parent_id(Some(previous_parent_node_id));

                self.nodes[path_node_id]
                    .remove_child_id(previous_parent_node_id);

                self.nodes[path_node_id].remove_child_id(node_id);

                previous_branch_attributes = temporary_branch_attributes;
                previous_branch_length = temporary_branch_length;
                previous_parent_node_id = path_node_id;
            }

            let mut new_node_label: Option<String> = self.nodes[left_node_id]
                .node_label()
                .map(|node_label| node_label.to_string());

            if let Some(yanked_node) = yanked_node_opt
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
                .set_branch_attributes(previous_branch_attributes.clone());

            let node_id_to_ignore: NodeId =
                if !path.is_empty() { path[path.len() - 1] } else { node_id };

            let temporary_child_node_ids =
                self.child_node_ids(left_node_id).to_vec();

            for child_node_id in temporary_child_node_ids {
                if child_node_id != node_id_to_ignore {
                    self.nodes[new_last].add_child_id(child_node_id);
                    self.nodes[child_node_id].set_parent_id(Some(new_last));
                }
            }
            _ = self.nodes.remove(left_node_id);
        }

        self.edges = None;
        let validation_result = self.validate()?;
        self.rebuild_edges();
        Ok(validation_result)
    }

    /// Converts a rooted tree to an unrooted tree by removing one root child node.
    ///
    /// This operation transforms a rooted tree into an unrooted tree by
    /// removing one of the root's two children. The branch length from the removed
    /// node is transferred to its sibling to preserve total distances in the tree.
    ///
    /// **Prerequisites:**
    /// - Tree must be rooted (`is_rooted()` returns `true`)
    /// - Root must have exactly two children
    /// - At least one root child must not be a tip
    ///
    /// **Operation process:**
    /// 1. **Node selection**: Selects which root child to remove using
    ///    `select_root_child_node_to_drop()`:
    ///    - If one child is a tip, drops the non-tip child
    ///    - If neither child is a tip, drops the one with more descending tips
    /// 2. **Branch length preservation**: Calls `slide_brlen_through_root()` to
    ///    transfer the source node's branch length to its sibling, preserving total
    ///    distance
    /// 3. **Node removal**: Calls `yank_node()` to remove the selected node and
    ///    connect its children directly to the root
    /// 4. **Tree maintenance**: Invalidates edges, validates the tree structure,
    ///    and rebuilds edges
    ///
    /// **Example transformation:**
    /// ```text
    /// Before unrooting (rooted):      After unrooting (unrooted):
    ///         Root                           FirstNode
    ///        /    \                         /  |  |  \
    ///    A(0.1)  B(0.2)          -->    A(0.3) F  G   H
    ///      /|\     /|\                    /|\
    ///     C D E   F G H                  C D E
    ///
    /// Node B is removed, and its branch length (0.2) is added to A's branch
    /// length (0.1), resulting in A having branch length 0.3. B's children
    /// (F, G, H) become direct children of the Root. The Root now has 4 children,
    /// making the tree unrooted.
    /// ```
    ///
    /// **Returns:**
    /// - `Ok(Node)` - The yanked node that was removed from the tree
    /// - `Err(TreeError::UnrootedTree)` - If the tree is already unrooted
    /// - `Err(TreeError::ChildNodeCountIsNotTwo)` - If root doesn't have exactly
    ///   two children
    /// - `Err(TreeError::BothChildNodesAreTips)` - If both root children are tips
    /// - `Err(TreeError::FailedToObtainMutableReferenceToNode)` - If nodes cannot
    ///   be accessed during the operation
    /// - Other `TreeError` variants from `yank_node()` or `validate()`
    ///
    /// **Note:**
    ///
    /// This operation modifies the tree structure and invalidates cached edges.
    /// The tree is automatically validated and edges are rebuilt after unrooting.
    pub fn unroot(&mut self) -> Result<Node, TreeError> {
        match self.select_root_child_node_to_drop() {
            Ok(node_to_drop_id) => {
                self.slide_brlen_through_root(node_to_drop_id);
                let yanked_node = self.yank_node(node_to_drop_id)?;
                self.edges = None;
                _ = self.validate()?;

                // Retain the label of the yanked node. ------------------------
                if let Some(yanked_node_label) = yanked_node.node_label()
                    && let Some(first_node) = self.node_mut(self.first_node_id)
                {
                    first_node.set_node_label(Some(yanked_node_label.as_ref()));
                } // -----------------------------------------------------------

                self.rebuild_edges();
                Ok(yanked_node)
            }
            Err(err) => Err(err),
        }
    }

    /// Removes a node from the tree while preserving the tree structure by
    /// connecting the node's children directly to its parent.
    ///
    /// This is a "yank" operation that pulls out a node from the tree topology
    /// while maintaining connectivity between the remaining nodes. All children
    /// of the yanked node become children of the yanked node's parent.
    ///
    /// **Operation Steps:**
    /// 1. Collect all child node IDs of the node to be yanked
    /// 2. Remove the yanked node from its parent's child list
    /// 3. Add all children of the yanked node to its parent's child list
    /// 4. Update the parent reference of all children to point to the yanked
    ///    node's parent
    /// 5. Remove the yanked node from the tree's node collection
    ///
    /// **Use Cases:**
    /// - Removing "knuckle" nodes (nodes with only one child) during tree
    ///   validation
    /// - Unrooting operations where a root child node needs to be removed
    /// - Tree restructuring operations that require node removal without
    ///   breaking connectivity
    ///
    /// **Example Tree Transformation:**
    /// ```text
    /// Before yanking node B:     After yanking node B:
    ///      A                           A
    ///      |                          /|\
    ///      B                         C D E
    ///     /|\
    ///    C D E
    /// ```
    ///
    /// **Arguments:**
    /// - `node_id` - The ID of the node to remove from the tree
    ///
    /// **Returns:**
    /// - `Ok(Node)` - The yanked node on success
    /// - `Err(TreeError::NodeDoesNotExist)` - If the node doesn't exist
    /// - `Err(TreeError::NodeDoesNotHaveParent)` - If the node has no parent
    /// - `Err(TreeError::FailedToObtainMutableReferenceToNode)` - If the parent
    ///   or any child node cannot be accessed via `get_mut()`
    ///
    /// **Safety:**
    ///
    /// This function uses safe node access patterns with `get_mut()`. If the
    /// parent node or any child nodes cannot be accessed via `get_mut()`, the
    /// function returns an error rather than silently skipping operations.
    ///
    /// **Note:**
    ///
    /// This operation does not preserve branch lengths or other node
    /// attributes.If branch length preservation is needed, it should be handled
    /// by the caller before calling this function.
    ///
    fn yank_node(&mut self, node_id: NodeId) -> Result<Node, TreeError> {
        if let Some(parent_node_id) = self.nodes[node_id].parent_id() {
            let child_node_ids = self.child_node_ids(node_id).to_owned();

            let yanked_node = self
                .nodes
                .remove(node_id)
                .ok_or(TreeError::NodeDoesNotExist(node_id))?;

            if let Some(parent_node) = self.nodes.get_mut(parent_node_id) {
                parent_node.remove_child_id(node_id);

                for &child_node_id in &child_node_ids {
                    parent_node.add_child_id(child_node_id);
                }
            } else {
                return Err(TreeError::FailedToObtainMutableReferenceToNode(
                    node_id,
                ));
            }

            for &child_node_id in &child_node_ids {
                if let Some(child_node) = self.nodes.get_mut(child_node_id) {
                    child_node.set_parent_id(Some(parent_node_id));
                } else {
                    return Err(
                        TreeError::FailedToObtainMutableReferenceToNode(
                            child_node_id,
                        ),
                    );
                }
            }
            Ok(yanked_node)
        } else {
            Err(TreeError::NodeDoesNotHaveParent(node_id))
        }
    }

    /// Redistributes branch lengths through the root node during unrooting operations.
    ///
    /// This function is specifically designed to preserve total distances
    /// when preparing a rooted tree for unrooting. It transfers the branch length from
    /// one root child (the "source" node that will be removed) to its sibling (the
    /// "receive" node that will remain), ensuring that no distance is lost
    /// during the unrooting process.
    ///
    /// **Prerequisites:**
    /// - Tree must be rooted (`is_rooted()` returns `true`)
    /// - Tree must have branch lengths (`has_branch_lengths` is `true`)
    /// - Both source and receive nodes must have branch lengths (function uses
    ///   `unwrap()`)
    ///
    /// **Operation process:**
    /// 1. **Validation**: Returns early if tree is unrooted or lacks branch lengths
    /// 2. **Sibling identification**: Finds the sibling of the source node among root's children
    /// 3. **Branch length calculation**: Combines source and receive node branch lengths
    /// 4. **Length redistribution**:
    ///    - Sets source node branch length to `Some(0.0)`
    ///    - Transfers combined length to the receive node
    ///
    /// **Branch length combination logic:**
    /// - Assumes both source and receive nodes have branch lengths (uses `unwrap()`).
    /// - Calculates: `new_length = source_length + receive_length`
    /// - Sets source branch length to `Some(0.0)` and receive branch length to `new_length`
    ///
    /// **Example transformation:**
    /// ```text
    /// Before sliding (rooted):        After sliding (preparing for unroot):
    ///         Root                             Root
    ///        /    \                           /    \
    ///    A(0.1)  B(0.2)          -->      A(0.0)  B(0.3)
    ///      /|\     /|\                      /|\     /|\
    ///     D E F   G H I                    D E F   G H I
    ///
    /// After A is yanked, B will connect directly to the tree with length 0.3,
    /// preserving the total distance: 0.1 + 0.2 = 0.3.
    /// ```
    ///
    /// **Usage context:**
    /// - Called exclusively during `unroot()` operation
    /// - Invoked before `yank_node()` to ensure proper branch length handling
    ///
    /// **Arguments:**
    /// - `source_node_id` - The root child node that will be removed (branch length donor)
    ///
    /// **Behavior:**
    /// - **Early return**: If tree is unrooted or lacks branch lengths
    /// - **Sibling detection**: Automatically identifies the other root child as receiver
    /// - **Safety check**: Returns early if source and receive nodes are the same
    /// - **Length preservation**: Ensures total distance is maintained
    ///
    /// **Safety:**
    /// - Uses safe `first_node_id()` access
    /// - Gracefully handles edge case where source and receive nodes are identical
    /// - Does not panic on invalid input, preferring early return for robustness
    ///
    /// **Note:**
    ///
    /// This function assumes the caller has already validated that the tree is in a
    /// valid state for unrooting (rooted tree with binary root). It's designed as a
    /// helper function for `unroot()` and should not be called independently.
    fn slide_brlen_through_root(&mut self, source_node_id: NodeId) {
        if !self.is_rooted() || !self.has_branch_lengths {
            return;
        }

        let mut receive_node_id: NodeId = source_node_id;
        if let Some(first_node_id) = self.first_node_id() {
            for &other_node_id in self.child_node_ids(first_node_id) {
                if other_node_id != source_node_id {
                    receive_node_id = other_node_id;
                    break;
                }
            }
        }

        if source_node_id == receive_node_id {
            println!(
                "slide_brlen_through_root: This should not have happened: source_node_id == receive_node_id."
            );
            return;
        }

        let source_node = &self.nodes[source_node_id];
        let receive_node = &self.nodes[receive_node_id];

        let source_brlen = source_node.branch_length().unwrap();
        let receive_brlen = receive_node.branch_length().unwrap();
        let new_brlen = receive_brlen + source_brlen;

        self.nodes[source_node_id].set_branch_length(Some(0.0));
        self.nodes[receive_node_id].set_branch_length(Some(new_brlen));
    }

    fn select_root_child_node_to_drop(&self) -> Result<NodeId, TreeError> {
        if !self.is_rooted() {
            return Err(TreeError::UnrootedTree);
        }
        if let Some(root_node) = self.node(self.first_node_id)
            && root_node.child_node_count() != 2
        {
            return Err(TreeError::ChildNodeCountIsNotTwo {
                node_id: root_node.node_id().unwrap(),
                child_node_count: root_node.child_node_count(),
            });
        }
        let root_child_nodes = self.child_nodes(self.first_node_id.unwrap());
        let child_node_1 = root_child_nodes[0];
        let child_node_2 = root_child_nodes[1];

        if child_node_1.is_tip() && child_node_2.is_tip() {
            return Err(TreeError::BothChildNodesAreTips {
                parent_node_id: self.first_node_id.unwrap(),
                child1_node_id: child_node_1.node_id().unwrap(),
                child2_node_id: child_node_2.node_id().unwrap(),
            });
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
                self.tip_node_count_recursive(child_node_1.node_id().unwrap());
            let tip_count_2 =
                self.tip_node_count_recursive(child_node_2.node_id().unwrap());

            if tip_count_1 > tip_count_2 {
                node_to_drop = child_node_1;
            } else {
                node_to_drop = child_node_2;
            }
        }

        Ok(node_to_drop.node_id().unwrap())
    }

    fn needs_edge_rebuild(&self) -> bool {
        self.edges.is_none()
    }

    pub(crate) fn rebuild_edges(&mut self) {
        if self.first_node_id.is_some() && self.needs_edge_rebuild() {
            let mut edges = prepare_edges(self);

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

    pub fn edges_within_tip_index_range(
        &self,
        tip_index_range: IndexRange,
    ) -> Option<&[Edge]> {
        let edges = self.edges()?;
        Some(&edges[tip_index_range])
    }

    pub fn bounding_tip_node_ids_for_clade(
        &'a self,
        node_id: NodeId,
    ) -> (NodeId, NodeId) {
        (
            self.first_tip_node_id_for_clade(node_id),
            self.last_tip_node_id_for_clade(node_id),
        )
    }

    pub fn first_tip_node_id_for_clade(&'a self, node_id: NodeId) -> NodeId {
        match self.first_child_node_id(node_id) {
            Some(child_id) => self.first_tip_node_id_for_clade(*child_id),
            None => node_id,
        }
    }

    pub fn last_tip_node_id_for_clade(&'a self, node_id: NodeId) -> NodeId {
        match self.last_child_node_id(node_id) {
            Some(child_id) => self.last_tip_node_id_for_clade(*child_id),
            None => node_id,
        }
    }

    pub fn bounding_node_ids_for_clade(
        &self,
        node_id: NodeId,
    ) -> Option<(Vec<NodeId>, Vec<NodeId>)> {
        if !self.node_exists(Some(node_id)) {
            return None;
        }

        let (top_tip_node_id, bottom_tip_node_id) =
            self.bounding_tip_node_ids_for_clade(node_id);

        let mut node_ids_top: Vec<NodeId> = Vec::new();
        let mut node_ids_bottom: Vec<NodeId> = Vec::new();

        let path_top = self.path(top_tip_node_id, node_id);
        let path_bottom = self.path(bottom_tip_node_id, node_id);

        node_ids_top.push(top_tip_node_id);

        for path_node_id in path_top {
            node_ids_top.push(path_node_id);
        }

        for &path_node_id in path_bottom.iter().rev() {
            node_ids_bottom.push(path_node_id);
        }

        node_ids_bottom.push(bottom_tip_node_id);

        Some((node_ids_top, node_ids_bottom))
    }

    pub fn bounding_edges_for_clade(
        &self,
        node_id: NodeId,
    ) -> Option<(Vec<Edge>, Vec<Edge>)> {
        let (node_ids_top, node_ids_bottom) =
            self.bounding_node_ids_for_clade(node_id)?;

        let edges = self.edges()?;

        let edges_top: Vec<Edge> = node_ids_top
            .iter()
            .filter_map(|&id| {
                self.edge_index_for_node_id(id)
                    .map(|edge_idx| edges[edge_idx].clone())
            })
            .collect();

        let edges_bottom: Vec<Edge> = node_ids_bottom
            .iter()
            .filter_map(|&id| {
                self.edge_index_for_node_id(id)
                    .map(|edge_idx| edges[edge_idx].clone())
            })
            .collect();

        Some((edges_top, edges_bottom))
    }

    pub fn bounding_tip_edge_index_range_for_clade(
        &self,
        node_id: NodeId,
    ) -> Option<IndexRange> {
        if !self.node_exists(Some(node_id)) {
            return None;
        }

        let (tip_node_id_1, tip_node_id_2) =
            self.bounding_tip_node_ids_for_clade(node_id);

        let tip_edge_idx_1: usize =
            self.edge_index_for_node_id(tip_node_id_1)?;
        let tip_edge_idx_2: usize =
            self.edge_index_for_node_id(tip_node_id_2)?;

        Some(IndexRange::new(tip_edge_idx_1, tip_edge_idx_2))
    }

    pub fn bounding_tip_edges_for_clade(
        &self,
        node_id: NodeId,
    ) -> Option<(&Edge, &Edge)> {
        let tip_edge_idx_range =
            self.bounding_tip_edge_index_range_for_clade(node_id)?;
        let edges = self.edges()?;
        let tip_edge_1 = &edges[*tip_edge_idx_range.start()];
        let tip_edge_2 = &edges[*tip_edge_idx_range.end()];
        Some((tip_edge_1, tip_edge_2))
    }

    pub fn sort(&mut self, reverse: bool) {
        if let Some(node_id) = self.first_node_id {
            self.nodes[node_id].set_node_order(0);
            self.sort_nodes(node_id, reverse, 1);
            self.edges = None;
            self.rebuild_edges();
        }
    }

    pub fn sorted_clone(tree: &Self, reverse: bool) -> Self {
        let mut tree_cloned = tree.clone();
        tree_cloned.sort(reverse);
        tree_cloned
    }

    fn sort_nodes(&mut self, node_id: NodeId, reverse: bool, _order: usize) {
        let mut sorted_ids: Vec<NodeId> =
            self.nodes[node_id].child_ids().to_vec();
        sorted_ids.par_sort_by_key(|&c| self.child_node_count_recursive(c));
        if reverse {
            sorted_ids.reverse();
        }
        for &child_node_id in &sorted_ids {
            self.sort_nodes(
                child_node_id,
                reverse,
                _order + sorted_ids.len() + 1,
            );
        }
        self.nodes[node_id].set_node_order(_order);
        self.nodes[node_id].set_child_ids(sorted_ids.clone());
        for (i, &node_id) in sorted_ids.iter().enumerate() {
            self.nodes[node_id].set_node_order(_order + i + 1);
        }
    }

    pub fn branch_attributes(
        &'a self,
        node_id: NodeId,
    ) -> &'a HashMap<String, Attribute> {
        self.nodes[node_id].branch_attributes()
    }

    pub fn node_attributes(
        &'a self,
        node_id: NodeId,
    ) -> &'a HashMap<String, Attribute> {
        self.nodes[node_id].node_attributes()
    }

    pub fn branch_attribute_keys(&self) -> Vec<String> {
        let mut result: HashSet<String> = HashSet::default();
        for node in self.nodes.values() {
            let attribute_keys: HashSet<String> = HashSet::from_iter(
                self.branch_attributes(node.node_id().unwrap()).keys().cloned(),
            );
            result.extend(attribute_keys);
        }
        result.into_iter().collect()
    }

    pub fn node_attribute_keys(&self) -> Vec<String> {
        let mut result: HashSet<String> = HashSet::default();
        for node in self.nodes.values() {
            let attribute_keys: HashSet<String> = HashSet::from_iter(
                self.node_attributes(node.node_id().unwrap()).keys().cloned(),
            );
            result.extend(attribute_keys);
        }
        result.into_iter().collect()
    }

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
            let mut node_attrs = node.node_attributes().clone();
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
            let mut branch_attrs = node.branch_attributes().clone();
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
    /// * `Err(TreeError::NodeDoesNotExist)` - If node doesn't exist
    /// * `Err(TreeError::AttributeNotFound)` - If attribute doesn't exist on node
    /// * `Err(TreeError::AttributeTypeMismatch)` - If new value type is incompatible
    pub fn change_node_attribute_value(
        &mut self,
        node_id: NodeId,
        key: &str,
        new_value: Attribute,
    ) -> Result<(), TreeError> {
        if !self.node_exists(Some(node_id)) {
            return Err(TreeError::NodeDoesNotExist(node_id));
        }

        // Check if the attribute exists on the node
        if !self
            .node(Some(node_id))
            .unwrap()
            .node_attributes()
            .contains_key(key)
        {
            return Err(TreeError::AttributeNotFound {
                attribute_key: key.to_string(),
                node_id,
            });
        }

        // Get the unified type for this attribute key across all nodes.
        let unified_type = self.get_unified_attribute_type_for_key(key, false);

        // Get mutable reference to the node.
        let node = self.node_mut(Some(node_id)).unwrap();

        if let Some(unified_type) = unified_type {
            // Validate that new_value can be converted to the unified type.
            let new_value_type = new_value.get_type();
            if !Attribute::can_unify_types(&new_value_type, &unified_type) {
                return Err(TreeError::AttributeTypeMismatch {
                    attribute_type_from: new_value_type,
                    attribute_type_to: unified_type,
                });
            }

            // Convert new_value if needed.
            let converted_value = if new_value_type != unified_type {
                Self::convert_attribute_to_type(new_value, &unified_type)?
            } else {
                new_value
            };

            // Update the node's attribute.
            let mut node_attrs = node.node_attributes().clone();
            let _ = node_attrs.insert(key.to_string(), converted_value);
            node.set_node_attributes(node_attrs);
        } else {
            // If no unified type exists, just update the attribute
            let mut node_attrs = node.node_attributes().clone();
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
    /// * `Err(TreeError::NodeDoesNotExist)` - If node doesn't exist
    /// * `Err(TreeError::AttributeNotFound)` - If attribute doesn't exist on node
    /// * `Err(TreeError::AttributeTypeMismatch)` - If new value type is incompatible
    pub fn change_branch_attribute_value(
        &mut self,
        node_id: NodeId,
        key: &str,
        new_value: Attribute,
    ) -> Result<(), TreeError> {
        if !self.node_exists(Some(node_id)) {
            return Err(TreeError::NodeDoesNotExist(node_id));
        }

        // Check if the attribute exists on the node
        if !self
            .node(Some(node_id))
            .unwrap()
            .branch_attributes()
            .contains_key(key)
        {
            return Err(TreeError::AttributeNotFound {
                attribute_key: key.to_string(),
                node_id,
            });
        }

        // Get the unified type for this attribute key across all nodes
        let unified_type = self.get_unified_attribute_type_for_key(key, true);

        // Get mutable reference to the node
        let node = self.node_mut(Some(node_id)).unwrap();

        if let Some(unified_type) = unified_type {
            // Validate that new_value can be converted to the unified type
            let new_value_type = new_value.get_type();
            if !Attribute::can_unify_types(&new_value_type, &unified_type) {
                return Err(TreeError::AttributeTypeMismatch {
                    attribute_type_from: new_value_type,
                    attribute_type_to: unified_type,
                });
            }

            // Convert new_value if needed
            let converted_value = if new_value_type != unified_type {
                Self::convert_attribute_to_type(new_value, &unified_type)?
            } else {
                new_value
            };

            // Update the node's attribute
            let mut branch_attrs = node.branch_attributes().clone();
            let _ = branch_attrs.insert(key.to_string(), converted_value);
            node.set_branch_attributes(branch_attrs);
        } else {
            // If no unified type exists, just update the attribute
            let mut branch_attrs = node.branch_attributes().clone();
            let _ = branch_attrs.insert(key.to_string(), new_value);
            node.set_branch_attributes(branch_attrs);
        }

        Ok(())
    }

    /// Gets the unified type for a specific attribute key across all nodes.
    fn get_unified_attribute_type_for_key(
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
            self.max_first_node_to_tip_distance(),
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
    fn fmt(&self, f: &mut Formatter<'_>) -> FormatterResult {
        write!(f, "{}", self.print_tree())
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    /// Helper function to create a simple tree structure for testing.
    /// Creates: Root -> Knuckle -> Child
    fn create_knuckle_test_tree() -> (Tree, NodeId, NodeId, NodeId) {
        let mut tree = Tree::new();

        // Create root node
        let root_id = tree.add_new_node(Some("root"), None, None).unwrap();

        // Create knuckle node (will have exactly one child)
        let knuckle_id = tree
            .add_new_node(Some("knuckle"), Some(0.1), Some(root_id))
            .unwrap();

        // Create child node
        let child_id = tree
            .add_new_node(Some("child"), Some(0.2), Some(knuckle_id))
            .unwrap();

        // Ensure the tree has branch lengths flag set
        tree.has_branch_lengths = true;

        (tree, root_id, knuckle_id, child_id)
    }

    /// Helper function to create a tree with multiple children (not a knuckle).
    fn create_non_knuckle_test_tree() -> (Tree, NodeId, NodeId) {
        let mut tree = Tree::new();

        // Create root node
        let root_id = tree.add_new_node(Some("root"), None, None).unwrap();

        // Create internal node with multiple children (not a knuckle)
        let internal_id = tree
            .add_new_node(Some("internal"), Some(0.1), Some(root_id))
            .unwrap();

        // Add multiple children to make it NOT a knuckle
        let _child1_id = tree
            .add_new_node(Some("child1"), Some(0.2), Some(internal_id))
            .unwrap();

        let _child2_id = tree
            .add_new_node(Some("child2"), Some(0.3), Some(internal_id))
            .unwrap();

        // Ensure the tree has branch lengths flag set
        tree.has_branch_lengths = true;

        (tree, root_id, internal_id)
    }

    #[test]
    fn test_remove_knuckle_with_both_labels() {
        let (mut tree, root_id, knuckle_id, child_id) =
            create_knuckle_test_tree();

        // Verify initial state
        assert_eq!(tree.label(knuckle_id).as_deref(), Some("knuckle"));
        assert_eq!(tree.label(child_id).as_deref(), Some("child"));
        assert_eq!(tree.branch_length(knuckle_id), Some(0.1));
        assert_eq!(tree.branch_length(child_id), Some(0.2));
        assert_eq!(tree.child_node_ids(knuckle_id).len(), 1);

        // Remove the knuckle
        tree.remove_knuckle(knuckle_id);

        // Verify knuckle node is removed
        assert!(!tree.node_exists(Some(knuckle_id)));

        // Verify child node is now connected to root
        assert_eq!(tree.parent_node_id(child_id), Some(root_id));
        assert!(tree.child_node_ids(root_id).contains(&child_id));

        // Verify branch length is combined
        let expected = 0.3; // 0.1 + 0.2
        let actual = tree.branch_length(child_id).unwrap();
        assert!(
            (actual - expected).abs() < TreeFloat::EPSILON,
            "Expected {}, got {}",
            expected,
            actual
        );

        // Verify label is combined
        assert_eq!(
            tree.label(child_id).as_deref(),
            Some("knuckle_KNUCKLE_child")
        );
    }

    #[test]
    fn test_remove_knuckle_only_knuckle_has_label() {
        let (mut tree, _root_id, knuckle_id, child_id) =
            create_knuckle_test_tree();

        // Remove child label
        if let Some(child_node) = tree.node_mut(Some(child_id)) {
            child_node.set_node_label(None::<&str>);
        }

        // Verify initial state
        assert_eq!(tree.label(knuckle_id).as_deref(), Some("knuckle"));
        assert_eq!(tree.label(child_id), None);

        // Remove the knuckle
        tree.remove_knuckle(knuckle_id);

        // Verify label is set correctly
        assert_eq!(tree.label(child_id).as_deref(), Some("knuckle_KNUCKLE"));
    }

    #[test]
    fn test_remove_knuckle_only_child_has_label() {
        let (mut tree, _root_id, knuckle_id, child_id) =
            create_knuckle_test_tree();

        // Remove knuckle label
        if let Some(knuckle_node) = tree.node_mut(Some(knuckle_id)) {
            knuckle_node.set_node_label(None::<&str>);
        }

        // Verify initial state
        assert_eq!(tree.label(knuckle_id), None);
        assert_eq!(tree.label(child_id).as_deref(), Some("child"));

        // Remove the knuckle
        tree.remove_knuckle(knuckle_id);

        // Verify label is set correctly
        assert_eq!(tree.label(child_id).as_deref(), Some("KNUCKLE_child"));
    }

    #[test]
    fn test_remove_knuckle_no_labels() {
        let (mut tree, root_id, knuckle_id, child_id) =
            create_knuckle_test_tree();

        // Remove both labels
        if let Some(knuckle_node) = tree.node_mut(Some(knuckle_id)) {
            knuckle_node.set_node_label(None::<&str>);
        }
        if let Some(child_node) = tree.node_mut(Some(child_id)) {
            child_node.set_node_label(None::<&str>);
        }

        // Verify initial state
        assert_eq!(tree.label(knuckle_id), None);
        assert_eq!(tree.label(child_id), None);

        // Remove the knuckle
        tree.remove_knuckle(knuckle_id);

        // Verify knuckle is removed and child is connected to root
        assert!(!tree.node_exists(Some(knuckle_id)));
        assert_eq!(tree.parent_node_id(child_id), Some(root_id));

        // Verify label remains None
        assert_eq!(tree.label(child_id), None);
    }

    #[test]
    fn test_remove_knuckle_branch_length_combinations() {
        // Test case 1: Both have branch lengths - knuckle is removed in first test
        let (mut tree, _root_id, knuckle_id, child_id) =
            create_knuckle_test_tree();
        assert_eq!(tree.branch_length(knuckle_id), Some(0.1));
        assert_eq!(tree.branch_length(child_id), Some(0.2));

        tree.remove_knuckle(knuckle_id);
        let expected = 0.3; // 0.1 + 0.2
        let actual = tree.branch_length(child_id).unwrap();
        assert!(
            (actual - expected).abs() < TreeFloat::EPSILON,
            "Expected {}, got {}",
            expected,
            actual
        );

        // Test case 2: Only knuckle has branch length
        let (mut tree2, _root_id2, knuckle_id2, child_id2) =
            create_knuckle_test_tree();

        // Remove child branch length
        if let Some(child_node) = tree2.node_mut(Some(child_id2)) {
            child_node.set_branch_length(None);
        }

        assert_eq!(tree2.branch_length(knuckle_id2), Some(0.1));
        assert_eq!(tree2.branch_length(child_id2), None);

        tree2.remove_knuckle(knuckle_id2);

        // Child branch length should remain None when child has no branch length
        assert_eq!(tree2.branch_length(child_id2), None);

        // Test case 3: Only child has branch length
        let (mut tree3, _root_id3, knuckle_id3, child_id3) =
            create_knuckle_test_tree();

        if let Some(knuckle_node) = tree3.node_mut(Some(knuckle_id3)) {
            knuckle_node.set_branch_length(None);
        }

        assert_eq!(tree3.branch_length(knuckle_id3), None);
        assert_eq!(tree3.branch_length(child_id3), Some(0.2));

        tree3.remove_knuckle(knuckle_id3);

        // Child branch length should remain unchanged when knuckle has no branch length
        assert_eq!(tree3.branch_length(child_id3), Some(0.2));
    }

    #[test]
    fn test_remove_knuckle_not_a_knuckle_no_children() {
        let mut tree = Tree::new();
        let leaf_id = tree.add_new_node(Some("leaf"), Some(0.1), None).unwrap();

        // Ensure the tree has branch lengths flag set
        tree.has_branch_lengths = true;

        // Verify it's a leaf (no children)
        assert_eq!(tree.child_node_ids(leaf_id).len(), 0);
        assert!(tree.node_exists(Some(leaf_id)));

        // Try to remove as knuckle - should do nothing
        tree.remove_knuckle(leaf_id);

        // Node should still exist since it's not a knuckle
        assert!(tree.node_exists(Some(leaf_id)));
        assert_eq!(tree.label(leaf_id).as_deref(), Some("leaf"));
    }

    #[test]
    fn test_remove_knuckle_not_a_knuckle_multiple_children() {
        let (mut tree, _root_id, internal_id) = create_non_knuckle_test_tree();

        // Verify it has multiple children (not a knuckle)
        assert_eq!(tree.child_node_ids(internal_id).len(), 2);
        assert!(tree.node_exists(Some(internal_id)));

        // Try to remove as knuckle - should do nothing
        tree.remove_knuckle(internal_id);

        // Node should still exist since it's not a knuckle
        assert!(tree.node_exists(Some(internal_id)));
        assert_eq!(tree.label(internal_id).as_deref(), Some("internal"));
        assert_eq!(tree.child_node_ids(internal_id).len(), 2);
    }

    #[test]
    fn test_remove_knuckle_child_becomes_direct_descendant() {
        let (mut tree, root_id, knuckle_id, child_id) =
            create_knuckle_test_tree();

        // Add grandchild to verify tree structure is maintained
        let grandchild_id = tree
            .add_new_node(Some("grandchild"), Some(0.3), Some(child_id))
            .unwrap();

        // Verify initial tree structure: root -> knuckle -> child -> grandchild
        assert_eq!(tree.parent_node_id(knuckle_id), Some(root_id));
        assert_eq!(tree.parent_node_id(child_id), Some(knuckle_id));
        assert_eq!(tree.parent_node_id(grandchild_id), Some(child_id));

        // Remove knuckle
        tree.remove_knuckle(knuckle_id);

        // Verify new tree structure: root -> child -> grandchild
        assert!(!tree.node_exists(Some(knuckle_id)));
        assert_eq!(tree.parent_node_id(child_id), Some(root_id));
        assert_eq!(tree.parent_node_id(grandchild_id), Some(child_id));
        assert!(tree.child_node_ids(root_id).contains(&child_id));
        assert!(tree.child_node_ids(child_id).contains(&grandchild_id));
    }

    #[test]
    fn test_remove_knuckle_with_no_branch_lengths_flag() {
        let (mut tree, root_id, knuckle_id, child_id) =
            create_knuckle_test_tree();

        // Set has_branch_lengths to false to test different behavior
        tree.has_branch_lengths = false;

        // Verify initial state
        assert_eq!(tree.label(knuckle_id).as_deref(), Some("knuckle"));
        assert_eq!(tree.label(child_id).as_deref(), Some("child"));
        assert_eq!(tree.child_node_ids(knuckle_id).len(), 1);

        // Remove the knuckle
        tree.remove_knuckle(knuckle_id);

        // Verify knuckle node is removed
        assert!(!tree.node_exists(Some(knuckle_id)));

        // Verify child node is now connected to root
        assert_eq!(tree.parent_node_id(child_id), Some(root_id));
        assert!(tree.child_node_ids(root_id).contains(&child_id));

        // Verify label is combined even when branch lengths are not used
        assert_eq!(
            tree.label(child_id).as_deref(),
            Some("knuckle_KNUCKLE_child")
        );
    }
}
