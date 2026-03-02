use dendros::parse_newick;
use std::collections::HashMap;
use std::time::Instant;

/// Performance demonstration example - parsing randomly generated trees with increasing tip count
fn main() {
    println!();

    // let tip_count_power_range = 15..=20;
    // let tip_counts: Vec<usize> =
    //     tip_count_power_range.map(|x| 2_usize.pow(x)).collect();

    let tip_counts: Vec<usize> = vec![
        5000, 10000, 50000, 100000, 250000, 500000, 750000, 1000000, 2000000,
    ];

    // Store timing results
    let mut results: HashMap<usize, f64> = HashMap::new();

    for (i, &tip_count) in tip_counts.iter().enumerate() {
        // Show progress on single updating line
        print!(
            "\r  [{}/{}] Processing {} tips...                              \r",
            i + 1,
            tip_counts.len(),
            tip_count
        );
        std::io::Write::flush(&mut std::io::stdout()).unwrap();

        // Generate random tree
        let newick = generate_random_tree(tip_count);

        // Measure parsing time
        let start = Instant::now();
        let parse_result = parse_newick(newick).ok();
        let duration = start.elapsed();

        match parse_result {
            Some(trees) => {
                let tree = &trees[0];
                let actual_nodes = tree.node_count_all();

                let duration_ms = duration.as_millis() as f64;
                let _ = results.insert(tip_count, duration_ms);

                // Update the same line with result
                print!(
                    "\r[{}/{}] {} tips → {} nodes, {:.2} ms",
                    i + 1,
                    tip_counts.len(),
                    tip_count,
                    actual_nodes,
                    duration_ms
                );
                std::io::Write::flush(&mut std::io::stdout()).unwrap();
            }
            None => {
                print!(
                    "\r[{}/{}] {} tips → Failed",
                    i + 1,
                    tip_counts.len(),
                    tip_count
                );
                std::io::Write::flush(&mut std::io::stdout()).unwrap();
            }
        }
    }

    // Clear the progress line and move to next line
    // println!();

    // Display performance summary
    println!(
        "\r=== === === === === === Performance Summary === === === === === ==="
    );
    display_performance_chart(&results);

    // Calculate and display performance metrics
    display_performance_metrics(&results);
}

/// Generate a random tree with the specified number of tips
fn generate_random_tree(tip_count: usize) -> String {
    if tip_count == 0 {
        return "();".to_string();
    }

    if tip_count == 1 {
        return "A;".to_string();
    }

    // Generate random labels for tips
    let mut tip_labels: Vec<String> =
        (1..=tip_count).map(|i| format!("Tip{:08}", i)).collect();

    // Shuffle the labels for randomness
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    // Simple shuffle using hash-based pseudo-randomness
    for i in 0..tip_labels.len() {
        let mut hasher = DefaultHasher::new();
        (i, tip_count).hash(&mut hasher);
        let hash = hasher.finish();
        let j = (hash as usize) % tip_labels.len();
        tip_labels.swap(i, j);
    }

    // Build tree structure recursively
    build_tree_structure(&tip_labels)
}

/// Build tree structure from tip labels using iterative approach to avoid stack overflow
fn build_tree_structure(tips: &[String]) -> String {
    if tips.is_empty() {
        return "();".to_string();
    }
    if tips.len() == 1 {
        return format!("{};", tips[0]);
    }

    // Use iterative approach with a queue to avoid stack overflow for large trees
    let mut nodes: Vec<String> = tips.to_vec();

    while nodes.len() > 1 {
        let mut next_level = Vec::new();

        for i in (0..nodes.len()).step_by(2) {
            if i + 1 < nodes.len() {
                // Pair two nodes
                next_level.push(format!("({},{})", nodes[i], nodes[i + 1]));
            } else {
                // Odd node out, carry it to next level
                next_level.push(nodes[i].clone());
            }
        }

        nodes = next_level;
    }

    format!("{};", nodes[0])
}

/// Display a simple terminal-based performance chart
fn display_performance_chart(results: &HashMap<usize, f64>) {
    if results.is_empty() {
        println!("No results to display.");
        return;
    }

    // Find min and max values for scaling
    let min_time = results.values().fold(f64::INFINITY, |a, &b| a.min(b));
    let max_time = results.values().fold(0.0_f64, |a, &b| a.max(b));

    if max_time == 0.0 {
        println!(
            "All parsing times were 0ms - too fast to measure accurately."
        );
        return;
    }

    println!("\nPerformance Chart (parsing time (ms) vs tip count)\n");

    let chart_height = 37;
    let mut sorted_results: Vec<_> = results.iter().collect();
    sorted_results.sort_by_key(|(tips, _)| **tips);

    // Determine column width based on the longest label
    let max_label_width = sorted_results
        .iter()
        .map(|(tips, _)| format!("{}", tips).len())
        .max()
        .unwrap_or(5);

    let col_width = max_label_width.max(2) + 1;

    // Calculate normalized bar heights for each data point
    let mut bar_heights = Vec::new();
    for (_tips, time) in &sorted_results {
        let normalized_height = if max_time > min_time {
            ((**time - min_time) / (max_time - min_time)
                * (chart_height - 1) as f64)
                .round() as usize
        } else {
            0
        };
        bar_heights.push(normalized_height);
    }

    // Print the chart from top to bottom
    for row in (0..chart_height).rev() {
        // Print time axis labels on the left
        let time_value = min_time
            + (max_time - min_time) * (row as f64 / (chart_height - 1) as f64);
        if row == chart_height - 1 {
            print!("{:10.0} ┤ ", max_time);
        } else if row == 0 {
            print!("{:10.0} ┤ ", min_time);
        } else if row % 4 == 0 {
            print!("{:10.0} ┤ ", time_value);
        } else {
            print!("{:>10} │ ", " ");
        }

        // Print vertical bars - each bar centered in its column
        for &bar_height in &bar_heights {
            // Calculate padding to center the bar character
            let padding_left = (col_width - 1) / 2;
            let padding_right = col_width - 1 - padding_left;

            if bar_height >= row {
                // Print bar with padding on both sides to fill the column
                print!(
                    "{:width_left$}█{:width_right$}",
                    "",
                    "",
                    width_left = padding_left,
                    width_right = padding_right
                );
            } else {
                // Print empty space with same total width
                print!("{:width$}", "", width = col_width);
            }
        }
        println!();
    }

    // Print bottom border - align with the bars (after "        │ ")
    print!("{:>10} └─", " ");
    for i in 0..sorted_results.len() {
        if i > 0 {
            print!("{}", "─".repeat(col_width));
        } else {
            print!("{}", "─".repeat(col_width - 1)); // First column: subtract 1 for the └─
        }
    }
    println!();

    // Print tip count labels, each centered in its column
    print!("{:>10}   ", " ");
    for (tips, _) in &sorted_results {
        print!("{:^width$}", tips, width = col_width);
    }
    println!();

    // Print "Tips" label
    // print!("{:>10}  ", " ");
    // let total_width = sorted_results.len() * col_width;
    // println!("{:^width$}", "Tips", width = total_width);
}

/// Display performance metrics and analysis
fn display_performance_metrics(results: &HashMap<usize, f64>) {
    if results.len() < 2 {
        println!("Need at least 2 data points for performance analysis.");
        return;
    }

    let mut sorted_results: Vec<_> = results.iter().collect();
    sorted_results.sort_by_key(|(tips, _)| **tips);

    println!(
        "\n{:>10} {:>12} {:>15} {:>15}",
        "Tips", "Time (ms)", "Time/Tip (μs)", "Growth Factor"
    );
    println!("{}", "─".repeat(75));

    let mut prev_time = 0.0;
    let mut _prev_tips = 0;

    for (tips, time) in &sorted_results {
        let time_per_tip =
            if **tips > 0 { (**time * 1000.0) / (**tips as f64) } else { 0.0 };

        let growth_factor =
            if prev_time > 0.0 { **time / prev_time } else { 1.0 };

        println!(
            "{:>8} {:>12.2} {:>15.2} {:>15.2}",
            tips, time, time_per_tip, growth_factor
        );

        prev_time = **time;
    }

    println!();

    // Check if it's roughly linear (O(n))
    let first_result = sorted_results[0];
    let last_result = sorted_results[sorted_results.len() - 1];

    let tips_ratio = *last_result.0 as f64 / *first_result.0 as f64;
    let time_ratio = *last_result.1 / *first_result.1;

    println!("Tips ratio (largest/smallest): {:.2}", tips_ratio);
    println!("Time ratio (largest/smallest): {:.2}", time_ratio);

    if time_ratio < tips_ratio * 1.5 {
        println!("Performance appears to be roughly linear O(n)");
    } else if time_ratio < tips_ratio * tips_ratio * 1.5 {
        println!("Performance appears to be roughly quadratic O(n²)");
    } else {
        println!("Performance appears to be worse than quadratic");
    }
}
