use super::{Attribute, NodeId, Tree, TreeFloat};
use rayon::prelude::*;
use std::{collections::HashMap, sync::Arc};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Edge {
    pub parent_node_id: Option<NodeId>,
    pub node_id: NodeId,
    pub label: Option<Arc<str>>,
    pub branch_length: TreeFloat,
    pub branch_length_normalized: TreeFloat,
    pub x0: TreeFloat,
    pub x_mid: TreeFloat,
    pub x1: TreeFloat,
    pub y_parent: Option<TreeFloat>,
    pub y: TreeFloat,
    pub is_tip: bool,
    pub edge_index: usize,
    pub node_attributes: HashMap<String, Attribute>,
    pub branch_attributes: HashMap<String, Attribute>,
}

pub(super) fn flatten_tree(tree: &Tree) -> Vec<Edge> {
    let tip_count = tree.tip_count_all();
    let tree_height = tree.height();
    let mut tip_id_counter = tip_count;
    if let Some(node_id) = &tree.first_node_id() {
        let (mut edges, _) = flatten_tree_recursive(
            *node_id, None, 0e0, tree, tree_height, tip_count,
            &mut tip_id_counter,
        );
        edges = calculate_vertical_positions(edges);
        edges.par_sort_by(|a, b| a.y.total_cmp(&b.y));
        edges
    } else {
        Vec::new()
    }
}

fn flatten_tree_recursive(
    node_id: NodeId,
    parent_node_id: Option<NodeId>,
    parent_height: TreeFloat,
    tree: &Tree,
    tree_height: TreeFloat,
    tip_count: usize,
    tip_id_counter: &mut usize,
) -> (Vec<Edge>, Vec<TreeFloat>) {
    let mut edges: Vec<Edge> = Vec::new();
    if tip_count == 0 {
        return (edges, Vec::new());
    }
    let branch_attributes = tree.branch_attributes(node_id);
    let node_attributes = tree.node_attributes(node_id);
    let branch_length: TreeFloat = tree.branch_length(node_id).unwrap_or(0e0);
    let branch_length_normalized: TreeFloat = branch_length / tree_height;
    let label: Option<Arc<str>> = tree.label(&node_id);
    let child_node_ids: &[NodeId] = tree.child_ids(&node_id);
    let descending_tip_count: usize = tree.tip_count_recursive(&node_id);
    let mut is_tip: bool = false;

    let mut y = TreeFloat::NAN;
    if descending_tip_count == 0 {
        *tip_id_counter -= 1;
        let tip_id = tip_count - *tip_id_counter;
        y = (tip_id - 1) as TreeFloat / (tip_count - 1) as TreeFloat;
        is_tip = true;
    }

    let node_height = parent_height + branch_length_normalized;
    let x_mid = parent_height.midpoint(node_height);

    let mut y_positions: Vec<TreeFloat> = Vec::new();
    for child_node_id in child_node_ids {
        let (mut child_edges, mut child_y_positions) = flatten_tree_recursive(
            *child_node_id,
            Some(node_id),
            node_height,
            tree,
            tree_height,
            tip_count,
            tip_id_counter,
        );

        edges.append(&mut child_edges);
        y_positions.append(&mut child_y_positions);
    }

    if !y.is_nan() {
        y_positions.push(y);
    } else if y_positions.is_empty() {
        // Single node case; no children.
        y = 0.0;
        y_positions = vec![y];
    } else {
        let y_min =
            y_positions.clone().into_iter().reduce(TreeFloat::min).unwrap();
        let y_max =
            y_positions.clone().into_iter().reduce(TreeFloat::max).unwrap();
        y = y_min.midpoint(y_max);
        y_positions = vec![y];
    }

    let this_edge: Edge = Edge {
        parent_node_id,
        node_id,
        label,
        branch_length,
        branch_length_normalized,
        x0: parent_height,
        x_mid,
        x1: node_height,
        y_parent: None,
        y,
        is_tip,
        edge_index: 0,
        node_attributes,
        branch_attributes,
    };

    edges.push(this_edge);

    (edges, y_positions)
}

fn calculate_vertical_positions(mut edges: Vec<Edge>) -> Vec<Edge> {
    let mut parent_y_positions: HashMap<NodeId, TreeFloat> = HashMap::new();

    for edge in &edges {
        if !edge.is_tip {
            _ = parent_y_positions.insert(edge.node_id, edge.y);
        }
    }

    for edge in &mut edges {
        if let Some(parent_id) = edge.parent_node_id {
            let parent_y = parent_y_positions[&parent_id];
            if parent_y != edge.y {
                edge.y_parent = Some(parent_y);
            }
        }
    }

    edges
}
