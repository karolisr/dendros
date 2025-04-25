use super::{Edges, TreeFloat};
use std::iter::zip;

#[derive(Debug, Default)]
pub struct LttPoint {
    pub time: TreeFloat,
    pub count: usize,
}

pub fn ltt(edges: &Edges, sample_count: usize, epsilon: TreeFloat) -> Vec<LttPoint> {
    let sample_size = 1e0 / sample_count as TreeFloat;
    let mut sample_counts: Vec<usize> = Vec::new();
    let mut sample_points: Vec<TreeFloat> = Vec::new();
    for sample_idx in 1..=sample_count {
        let sample_point = sample_idx as TreeFloat * sample_size;
        let mut sample_count: usize = 0;
        let mut sample_edges: Edges = Vec::new();
        for e in edges {
            if e.x0 < sample_point && e.x1 >= sample_point - epsilon {
                sample_count += 1;
                sample_edges.push(e.clone());
            }
        }
        sample_points.push(sample_point);
        sample_counts.push(sample_count);
    }

    let points: Vec<(TreeFloat, usize)> = zip(sample_points, sample_counts).collect();
    let points: Vec<LttPoint> = points
        .iter()
        .map(|a| LttPoint { time: a.0, count: a.1 })
        .collect();

    points
}
