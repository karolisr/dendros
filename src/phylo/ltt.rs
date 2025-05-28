use super::{Edge, TreeFloat};
use std::iter::zip;

#[derive(Debug, Default)]
pub struct LttPoint {
    pub time: TreeFloat,
    pub count: usize,
}

pub fn ltt(edges: &Vec<Edge>, sample_count: usize) -> Vec<LttPoint> {
    let sample_every = 1e0 / sample_count as TreeFloat;
    let mut counts: Vec<usize> = Vec::new();
    let mut sample_points: Vec<TreeFloat> = Vec::new();
    for sample_idx in 1..=sample_count {
        let sample_point = sample_idx as TreeFloat * sample_every;
        let mut current_count: usize = 0;
        for e in edges {
            if e.x0 < sample_point && e.x1 >= sample_point - sample_every / 1e1 {
                current_count += 1;
            }
        }
        sample_points.push(sample_point);
        counts.push(current_count);
    }

    let points: Vec<(TreeFloat, usize)> = zip(sample_points, counts).collect();
    let points: Vec<LttPoint> = points.iter().map(|a| LttPoint { time: a.0, count: a.1 }).collect();

    points
}
