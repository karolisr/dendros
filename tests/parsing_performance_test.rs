use dendros::parse_newick;

#[test]
fn test_large_tree_parsing_performance() {
    // Test validation on truly large trees (>10,000 nodes) to ensure performance is reasonable
    let large_tree_sizes = vec![1000, 5000, 10000, 15000];

    for size in large_tree_sizes {
        println!("Testing large tree validation: {} tips", size);

        // Generate a balanced binary tree string
        let newick_str = generate_balanced_tree_string(size);

        let start_time = std::time::Instant::now();
        let trees = parse_newick(newick_str)
            .unwrap_or_else(|| panic!("Failed to parse {}-tip tree", size));
        let parse_duration = start_time.elapsed();

        let tree = &trees[0];

        let validation_start = std::time::Instant::now();

        // Validate basic properties
        assert_eq!(
            tree.tip_count_all(),
            size,
            "Wrong tip count for {}-tip tree",
            size
        );

        let expected_total_nodes = if size == 1 { 1 } else { 2 * size - 1 };
        assert_eq!(
            tree.node_count_all(),
            expected_total_nodes,
            "Wrong total node count for {}-tip tree",
            size
        );

        // Calculate tree height (should be log2(size) for balanced binary tree)
        let height = calculate_tree_height(tree);

        // Validate structure makes sense
        assert!(height > 0.0, "Tree height should be positive");

        let validation_duration = validation_start.elapsed();

        println!(
            "{}-tip tree: parse={:.2}ms, validation={:.2}ms, height={:.1}",
            size,
            parse_duration.as_millis(),
            validation_duration.as_millis(),
            height
        );

        // Performance check: parsing + validation should be reasonably fast even for large trees
        let total_time = parse_duration + validation_duration;
        let max_time_ms = if size >= 10000 {
            30000
        } else if size >= 5000 {
            15000
        } else {
            5000
        };
        assert!(
            total_time.as_millis() < max_time_ms,
            "Performance issue: {}-tip tree took {}ms (max allowed: {}ms)",
            size,
            total_time.as_millis(),
            max_time_ms
        );
    }
}

/// Calculate tree height (maximum distance from root to any tip)
fn calculate_tree_height(tree: &dendros::Tree) -> f64 {
    if let Some(root_id) = tree.first_node_id() {
        let tip_ids = tree.tip_node_ids_all();
        let mut max_distance: f64 = 0.0;

        for tip_id in tip_ids {
            let distance = tree.distance(&root_id, &tip_id);
            max_distance = max_distance.max(distance);
        }

        max_distance
    } else {
        0.0
    }
}

/// Generate a balanced binary tree NEWICK string with the specified number of tips
fn generate_balanced_tree_string(n: usize) -> String {
    if n == 1 {
        return "A:1.0;".to_string();
    }

    fn generate_subtree(start: usize, count: usize) -> String {
        if count == 1 {
            format!("T{}:0.1", start)
        } else if count == 2 {
            format!("(T{}:0.1,T{}:0.1):0.1", start, start + 1)
        } else {
            let mid = count / 2;
            let left = generate_subtree(start, mid);
            let right = generate_subtree(start + mid, count - mid);
            format!("({}:0.1,{}:0.1):0.1", left, right)
        }
    }

    format!("{};", generate_subtree(1, n))
}

#[test]
fn test_large_multifurcation_parsing_performance() {
    // Test truly large multifurcating trees (>10,000 nodes)
    println!("Testing large multifurcating tree parsing");

    let large_multifurcation = generate_large_multifurcation_tree(8000); // 8000 tips = 8001 nodes
    let start_time = std::time::Instant::now();

    let trees = parse_newick(large_multifurcation)
        .expect("Failed to parse large multifurcating tree");

    let parse_duration = start_time.elapsed();
    let tree = &trees[0];

    // Validate the structure
    assert_eq!(tree.tip_count_all(), 8000, "Should have 8000 tips");
    assert_eq!(
        tree.node_count_all(),
        8001,
        "Should have 8001 total nodes (8000 tips + 1 root)"
    );

    // Performance check - should parse reasonably quickly
    assert!(
        parse_duration.as_millis() < 10000,
        "Large multifurcation parsing took too long: {}ms",
        parse_duration.as_millis()
    );

    println!(
        "8000-tip multifurcating tree parsed in {}ms",
        parse_duration.as_millis()
    );
}

/// Generate a large multifurcating tree NEWICK string with the specified number of tips
fn generate_large_multifurcation_tree(n_tips: usize) -> String {
    let mut tips = Vec::with_capacity(n_tips);
    for i in 1..=n_tips {
        tips.push(format!("T{}", i));
    }
    format!("({});", tips.join(","))
}
