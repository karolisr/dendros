use super::{NodeId, Tree, TreeFloat};
use std::{collections::HashMap, sync::Arc};

pub type Edges = Vec<Edge>;

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub struct Edge {
    pub parent_node_id: Option<NodeId>,
    pub node_id: NodeId,
    pub name: Option<Arc<str>>,
    pub brlen: TreeFloat,
    pub brlen_normalized: TreeFloat,
    pub x0: TreeFloat,
    pub x1: TreeFloat,
    pub y_parent: Option<TreeFloat>,
    pub y: TreeFloat,
    pub is_tip: bool,
    pub chunk_idx: usize,
    pub edge_idx: usize,
}

pub fn flatten_tree(tree: &Tree) -> Edges {
    let ntip = tree.tip_count_all();
    let tree_height = tree.height();
    let mut tip_id_counter = ntip;
    if let Some(id) = tree.first_node_id() {
        let (mut edges, _) =
            _flatten_tree(id, None, 0e0, tree, tree_height, ntip, &mut tip_id_counter);
        edges = calc_verticals(edges);
        edges.sort_by(|a, b| a.y.total_cmp(&b.y));
        edges
    } else {
        Vec::new()
    }
}

fn _flatten_tree(
    node_id: NodeId,
    parent_node_id: Option<NodeId>,
    node_height: TreeFloat,
    tree: &Tree,
    tree_height: TreeFloat,
    ntip: usize,
    tip_id_counter: &mut usize,
) -> (Edges, Vec<TreeFloat>) {
    let mut edges: Edges = Vec::new();
    if ntip == 0 {
        return (edges, Vec::new());
    }
    let brlen: TreeFloat = tree.branch_length(node_id).unwrap_or(0e0);
    let brlen_normalized: TreeFloat = brlen / tree_height;
    let name: Option<Arc<str>> = tree.name(node_id);
    let child_node_ids: &[NodeId] = tree.child_ids(node_id);
    let descending_tip_count: usize = tree.tip_count_recursive(node_id);
    let mut is_tip: bool = false;

    let mut y = TreeFloat::NAN;
    if descending_tip_count == 0 {
        *tip_id_counter -= 1;
        let tip_id = ntip - *tip_id_counter;
        y = (tip_id - 1) as TreeFloat / (ntip - 1) as TreeFloat;
        is_tip = true;
    }

    let mut ys: Vec<TreeFloat> = Vec::new();
    for &child_node_id in child_node_ids {
        let (mut child_edges, mut child_ys) = _flatten_tree(
            child_node_id,
            Some(node_id),
            node_height + brlen_normalized,
            tree,
            tree_height,
            ntip,
            tip_id_counter,
        );

        edges.append(&mut child_edges);
        ys.append(&mut child_ys);
    }

    if !y.is_nan() {
        ys.push(y);
    } else {
        let y_min = ys.clone().into_iter().reduce(TreeFloat::min).unwrap();
        let y_max = ys.clone().into_iter().reduce(TreeFloat::max).unwrap();
        y = (y_max - y_min) / 2e0 + y_min;
        ys = vec![y];
    }

    let this_edge: Edge = Edge {
        parent_node_id,
        node_id,
        name,
        brlen,
        brlen_normalized,
        x0: node_height,
        x1: node_height + brlen_normalized,
        y_parent: None,
        y,
        is_tip,
        chunk_idx: 0,
        edge_idx: 0,
    };

    edges.push(this_edge);

    (edges, ys)
}

fn calc_verticals(mut edges: Edges) -> Edges {
    let mut p_ys: HashMap<NodeId, TreeFloat> = HashMap::new();

    for e in &edges {
        if !e.is_tip {
            p_ys.insert(e.node_id, e.y);
        }
    }

    for e in &mut edges {
        if let Some(p) = e.parent_node_id {
            let p_y = p_ys[&p];
            if p_y != e.y {
                e.y_parent = Some(p_y);
            }
        }
    }

    edges
}

pub fn chunk_edges(edges: &Edges, chunk_count: usize) -> Vec<Edges> {
    let edge_count = edges.len();
    if edge_count == 0 {
        return Vec::new();
    }
    let mut chunk_count = chunk_count;
    if chunk_count < 2 {
        chunk_count = 1;
    }
    let edge_count_per_chunk = edge_count / chunk_count;
    let remainder = edge_count % chunk_count;
    let mut chunks: Vec<Vec<Edge>> = Vec::new();
    for t in 0..chunk_count {
        let i1 = edge_count_per_chunk * t;
        let i2 = edge_count_per_chunk * (t + 1);
        let edges = &edges[i1..i2];
        chunks.push(edges.to_vec());
    }
    if remainder > 0 {
        let edges = &edges[edge_count_per_chunk * chunk_count..];
        chunks.push(edges.to_vec());
    }
    chunks
}
