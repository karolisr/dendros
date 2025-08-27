// Example modified from a "Claude Sonnet 4" generated code.

use dendros::{ltt, parse_newick};
use std::fs::read_to_string;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=========================================\n");

    // Example 1: Simple tree from Newick string
    println!("Example 1: Simple tree");
    let simple_tree_newick = "(Nine:0.85,((Five:0.5,Four:0.4,(Two:0.2,One:0.1)Three:0.3)Six:0.6,Seven:0.7):0.85);";
    analyze_tree("Simple tree", simple_tree_newick, 20)?;

    println!("\n{}\n", "=".repeat(50));

    // Example 2: More complex tree from file
    println!("Example 2: Complex tree from file");
    let complex_tree_data = read_to_string("tests/data/tree02.newick")?;
    analyze_tree("Tree from file", &complex_tree_data, 30)?;

    println!("\n{}\n", "=".repeat(50));

    // Example 3: Very large tree (if available)
    if let Ok(large_tree_data) =
        read_to_string("tests/data/100_starting_trees.newick")
    {
        println!(
            "Example 3: Large phylogenetic tree (first tree from 100 starting trees)"
        );
        // Only use the first tree from the file
        let first_tree_line = large_tree_data.lines().next().unwrap_or("");
        if !first_tree_line.is_empty() {
            analyze_tree("Large phylogenetic tree", first_tree_line, 50)?;
        }
    }

    println!("\n{}", "=".repeat(50));

    Ok(())
}

fn analyze_tree(
    name: &str,
    newick_data: &str,
    sample_points: usize,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("Analyzing: {}", name);

    // Parse the Newick string
    let mut trees = parse_newick(newick_data.to_string())
        .ok_or("Failed to parse Newick data")?;

    if trees.is_empty() {
        println!("No trees found in the data");
        return Ok(());
    }

    let tree = &mut trees[0];

    // Print basic tree information
    println!("Tree height: {:.3}", tree.height());
    println!("Number of tips: {}", tree.tip_count_all());

    // Unroot the tree if needed and prepare edges
    if tree.is_rooted() {
        let _ = tree.unroot();
        println!("Tree was rooted, unrooted for analysis");
    }
    tree.make_fresh_edges();

    // Get edges for LTT analysis
    let edges = tree.edges().ok_or("Failed to generate edges")?;
    println!("Number of edges: {}", edges.len());

    // Generate LTT plot data
    let ltt_points = ltt(tree.height(), edges, sample_points);

    println!("\nLineages-Through-Time data (Height -> Lineage Count):");
    println!("{:>8} | {:>8} | Visual", "Height", "Lineages");
    println!("{}", "-".repeat(40));

    // Find max lineage count for scaling the visualization
    let max_lineages = ltt_points.iter().map(|p| p.count).max().unwrap_or(1);
    let scale_factor = 30.0 / max_lineages as f32; // Scale to fit in ~30 characters

    for point in &ltt_points {
        let bar_length = (point.count as f32 * scale_factor) as usize;
        let bar = "█".repeat(bar_length);
        println!("{:>8.3} | {:>8} | {}", point.height, point.count, bar);
    }

    // Show some summary statistics
    if let (Some(min_lineages), Some(max_lineages)) = (
        ltt_points.iter().map(|p| p.count).min(),
        ltt_points.iter().map(|p| p.count).max(),
    ) {
        println!("\nSummary:");
        println!("Min lineages: {}", min_lineages);
        println!("Max lineages: {}", max_lineages);
        println!("Lineage range: {}", max_lineages - min_lineages);

        // Calculate diversity through time
        let total_height = tree.height();
        let mid_point_idx = ltt_points.len() / 2;
        let mid_lineages = ltt_points[mid_point_idx].count;

        println!(
            "Lineages at tree base: {}",
            ltt_points.last().map(|p| p.count).unwrap_or(0)
        );
        println!(
            "Lineages at mid-point ({:.2}): {}",
            total_height / 2.0,
            mid_lineages
        );
        println!(
            "Lineages at tree tips: {}",
            ltt_points.first().map(|p| p.count).unwrap_or(0)
        );
    }

    Ok(())
}
