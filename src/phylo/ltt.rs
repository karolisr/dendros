use super::edges::Edge;
use crate::TreeFloat;

use std::iter::zip;

#[derive(Debug, Default)]
pub struct LttPoint {
    pub height: TreeFloat,
    pub count: usize,
}

pub fn ltt(
    tree_height: TreeFloat,
    edges: &Vec<Edge>,
    sample_count: usize,
) -> Vec<LttPoint> {
    let sample_every = 1e0 / sample_count as TreeFloat;
    let mut counts: Vec<usize> = Vec::new();
    let mut heights: Vec<TreeFloat> = Vec::new();

    for sample_idx in 1..=sample_count {
        let relative_position = sample_idx as TreeFloat * sample_every;
        let mut current_count: usize = 0;
        for edge in edges {
            if edge.x0 < relative_position
                && edge.x1 >= relative_position - sample_every / 1e1
            {
                current_count += 1;
            }
        }
        heights.push(relative_position * tree_height);
        counts.push(current_count);
    }

    zip(heights, counts)
        .map(|(height, count)| LttPoint { height, count })
        .collect()
}
