use dendros::{Tree, parse_newick, parse_nexus_advanced};

#[test]
fn test_tree_topology_and_structure_validation() {
    let test_cases = vec![
        // Basic topologies
        (
            "Simple bifurcating",
            "(A,B);",
            TreeTopology {
                tip_count: 2,
                internal_count: 1,
                total_count: 3,
                is_binary: true,
                max_depth: 1,
            },
        ),
        (
            "Three-tip tree",
            "(A,B,C);",
            TreeTopology {
                tip_count: 3,
                internal_count: 1,
                total_count: 4,
                is_binary: false, // Root has 3 children - POLYTOMY (trifurcation)
                max_depth: 1,
            },
        ),
        (
            "Nested binary",
            "((A,B),(C,D));",
            TreeTopology {
                tip_count: 4,
                internal_count: 3,
                total_count: 7,
                is_binary: true,
                max_depth: 2,
            },
        ),
        (
            "Asymmetric tree",
            "(A,(B,(C,D)));",
            TreeTopology {
                tip_count: 4,
                internal_count: 3,
                total_count: 7,
                is_binary: true,
                max_depth: 3,
            },
        ),
        (
            "Medium multifurcation",
            "(A,B,C,D,E,F);",
            TreeTopology {
                tip_count: 6,
                internal_count: 1,
                total_count: 7,
                is_binary: false, // POLYTOMY: Star tree (6-way multifurcation)
                max_depth: 1,
            },
        ),
        (
            "Mixed binary/multifurcating",
            "((A,B,C),(D,E));",
            TreeTopology {
                tip_count: 5,
                internal_count: 3,
                total_count: 8,
                is_binary: false, // POLYTOMY: One internal node has 3 children (trifurcation)
                max_depth: 2,
            },
        ),
    ];

    for (name, newick_str, expected) in test_cases {
        println!("Testing topology: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];
        validate_tree_topology(tree, &expected, name);

        println!(
            "Topology validated: {} tips, {} internals, depth {}",
            expected.tip_count, expected.internal_count, expected.max_depth
        );
    }
}

#[test]
fn test_tree_height_calculation_and_tip_distances() {
    let test_cases = vec![
        // Trees with branch lengths for height calculation
        ("Equal branch lengths", "(A:1.0,B:1.0);", 1.0, vec![1.0, 1.0]),
        ("Different tip distances", "(A:0.5,B:1.5);", 1.5, vec![0.5, 1.5]),
        (
            "Nested with heights",
            "((A:0.1,B:0.2):0.8,C:1.0);",
            1.0,
            vec![0.9, 1.0, 1.0],
        ),
        (
            "Complex tree",
            "((A:0.1,B:0.1):0.2,(C:0.15,D:0.05):0.15);",
            0.3,
            vec![0.2, 0.3, 0.3, 0.3],
        ),
        (
            "Ultrametric tree",
            "((A:0.5,B:0.5):0.5,(C:0.5,D:0.5):0.5);",
            1.0,
            vec![1.0, 1.0, 1.0, 1.0],
        ),
    ];

    for (name, newick_str, expected_height, expected_tip_distances) in
        test_cases
    {
        println!("Testing tree heights: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];

        // Calculate tree height (maximum distance from root to any tip)
        let actual_height = calculate_tree_height(tree);
        assert!(
            (actual_height - expected_height).abs() < 1e-6,
            "Tree height mismatch for {}: expected {}, got {}",
            name,
            expected_height,
            actual_height
        );

        // Check tip distances
        let tip_distances = calculate_tip_distances(tree);
        assert_eq!(
            tip_distances.len(),
            expected_tip_distances.len(),
            "Wrong number of tip distances for {}",
            name
        );

        for (i, (actual, expected)) in
            tip_distances.iter().zip(expected_tip_distances.iter()).enumerate()
        {
            assert!(
                (actual - expected).abs() < 1e-6,
                "Tip distance {} mismatch for {}: expected {}, got {}",
                i,
                name,
                expected,
                actual
            );
        }

        println!(
            "Height: {:.3}, Tip distances: {:?}",
            actual_height, tip_distances
        );
    }
}

#[test]
fn test_tree_rooting_status_detection() {
    let test_cases = vec![
        // Test rooting detection
        ("Unrooted 3-tip", "(A,B,C);", false, 3), // Unrooted: root has 3 children
        ("Unrooted 4-tip", "(A,B,C,D);", false, 4), // Unrooted: root has 4 children
        ("Rooted binary", "((A,B),(C,D));", true, 4), // Rooted: root has 2 children
        ("Rooted asymmetric", "(A,(B,C));", true, 3), // Rooted: root has 2 children
        // Rich NEWICK rooting annotations
        ("[&U] unrooted", "[&U](A,B,C);", false, 3),
        ("[&R] rooted", "[&R](A,B,C);", true, 3), // Forced rooted
    ];

    for (name, newick_str, expected_rooted, expected_tips) in test_cases {
        println!("Testing rooting: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];

        assert_eq!(
            tree.tip_count_all(),
            expected_tips,
            "Wrong tip count for {}: expected {}, got {}",
            name,
            expected_tips,
            tree.tip_count_all()
        );

        // Check rooting status
        let is_rooted = tree.is_rooted();
        assert_eq!(
            is_rooted, expected_rooted,
            "Wrong rooting status for {}: expected {}, got {}",
            name, expected_rooted, is_rooted
        );

        println!("Tips: {}, Rooted: {}", expected_tips, is_rooted);
    }
}

#[test]
fn test_branch_length_analysis_and_properties() {
    let test_cases = vec![
        (
            "No branch lengths",
            "(A,B,C);",
            BranchLengthStats {
                has_any: false,
                has_all: false,
                min_length: None,
                max_length: None,
                total_length: 0.0,
            },
        ),
        (
            "All branch lengths",
            "(A:1.0,B:2.0,C:3.0);",
            BranchLengthStats {
                has_any: true,
                has_all: true,
                min_length: Some(1.0),
                max_length: Some(3.0),
                total_length: 6.0,
            },
        ),
        (
            "Partial branch lengths",
            "(A:1.0,B,C:2.0);",
            BranchLengthStats {
                has_any: true,
                has_all: false,
                min_length: Some(1.0),
                max_length: Some(2.0),
                total_length: 3.0,
            },
        ),
        (
            "Zero branch lengths",
            "(A:0.0,B:0.0);",
            BranchLengthStats {
                has_any: true,
                has_all: true,
                min_length: Some(0.0),
                max_length: Some(0.0),
                total_length: 0.0,
            },
        ),
        (
            "Complex with internals",
            "((A:0.1,B:0.2):0.3,C:0.4);",
            BranchLengthStats {
                has_any: true,
                has_all: true,
                min_length: Some(0.1),
                max_length: Some(0.4),
                total_length: 1.0,
            },
        ),
    ];

    for (name, newick_str, expected) in test_cases {
        println!("Testing branch lengths: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];
        let stats = calculate_branch_length_stats(tree);

        assert_eq!(
            stats.has_any, expected.has_any,
            "has_any mismatch for {}",
            name
        );
        assert_eq!(
            stats.has_all, expected.has_all,
            "has_all mismatch for {}",
            name
        );

        match (stats.min_length, expected.min_length) {
            (Some(actual), Some(expected)) => {
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "min_length mismatch for {}: expected {}, got {}",
                    name,
                    expected,
                    actual
                );
            }
            (None, None) => {} // Both None, OK
            _ => panic!("min_length option mismatch for {}", name),
        }

        match (stats.max_length, expected.max_length) {
            (Some(actual), Some(expected)) => {
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "max_length mismatch for {}: expected {}, got {}",
                    name,
                    expected,
                    actual
                );
            }
            (None, None) => {} // Both None, OK
            _ => panic!("max_length option mismatch for {}", name),
        }

        assert!(
            (stats.total_length - expected.total_length).abs() < 1e-6,
            "total_length mismatch for {}: expected {}, got {}",
            name,
            expected.total_length,
            stats.total_length
        );

        println!(
            "Branch lengths: has_any={}, has_all={}, total={:.3}",
            stats.has_any, stats.has_all, stats.total_length
        );
    }
}

#[test]
fn test_node_labeling_and_property_analysis() {
    let test_cases = vec![
        ("No labels", "(,,);", 0, 0), // No tip or internal labels
        ("Tip labels only", "(A,B,C);", 3, 0), // 3 tip labels, 0 internal
        ("All labels", "(A,B,C)Root;", 3, 1), // 3 tips + 1 internal
        ("Mixed labels", "((A,B)Internal,C);", 3, 1), // 3 tips + 1 internal
        ("Quoted labels", "('Taxon A','Taxon B');", 2, 0), // 2 quoted tip labels
        ("Labels with attributes", "(A[&color=red],B[&type=mammal]);", 2, 0), // Tips with attributes
    ];

    for (name, newick_str, expected_tip_labels, expected_internal_labels) in
        test_cases
    {
        println!("Testing node labels: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];

        // Count tip labels
        let tip_label_count = tree
            .tip_node_ids_all()
            .iter()
            .filter(|&&id| {
                tree.node(Some(id))
                    .and_then(dendros::Node::node_label)
                    .is_some()
            })
            .count();

        assert_eq!(
            tip_label_count, expected_tip_labels,
            "Tip label count mismatch for {}: expected {}, got {}",
            name, expected_tip_labels, tip_label_count
        );

        // Count internal node labels
        let internal_label_count = tree
            .node_ids_all()
            .iter()
            .filter(|&&id| {
                !tree.is_tip(&id)
                    && tree
                        .node(Some(id))
                        .and_then(dendros::Node::node_label)
                        .is_some()
            })
            .count();

        assert_eq!(
            internal_label_count, expected_internal_labels,
            "Internal label count mismatch for {}: expected {}, got {}",
            name, expected_internal_labels, internal_label_count
        );

        println!(
            "Tip labels: {}, Internal labels: {}",
            tip_label_count, internal_label_count
        );
    }
}

#[test]
fn test_nexus_tree_analysis_with_translation_tables() {
    // Test NEXUS-specific validation including translate table effects
    let nexus_content = r#"
#NEXUS

Begin Trees;
    Translate
        1 Very_Long_Species_Name_A,
        2 Very_Long_Species_Name_B,
        3 Very_Long_Species_Name_C,
        4 Very_Long_Species_Name_D
    ;

    Tree symmetric = ((1:0.1,2:0.1):0.2,(3:0.1,4:0.1):0.2);
    Tree asymmetric = (1:0.4,(2:0.1,(3:0.05,4:0.05):0.05):0.3);
End;
    "#;

    println!("Testing NEXUS tree validation with translate");
    let result =
        parse_nexus_advanced(nexus_content).expect("Should parse successfully");

    // Test symmetric tree
    let symmetric_tree =
        result.tree("symmetric").expect("Should have symmetric tree");

    // Validate topology
    assert_eq!(
        symmetric_tree.tip_count_all(),
        4,
        "Symmetric tree should have 4 tips"
    );
    assert_eq!(
        symmetric_tree.node_count_all(),
        7,
        "Symmetric tree should have 7 total nodes"
    );
    assert!(symmetric_tree.is_rooted(), "Symmetric tree should be rooted");

    // Validate height (should be 0.3 for both sides)
    let height = calculate_tree_height(symmetric_tree);
    assert!((height - 0.3).abs() < 1e-6, "Symmetric tree height should be 0.3");

    // Validate that all tips have translated names
    let tip_labels: Vec<String> = symmetric_tree
        .tip_node_ids_all()
        .iter()
        .filter_map(|&id| {
            symmetric_tree.node(Some(id))?.node_label().map(|l| l.to_string())
        })
        .collect();

    assert_eq!(tip_labels.len(), 4, "Should have 4 tip labels");
    for label in &tip_labels {
        assert!(
            label.starts_with("Very Long Species Name"),
            "Tip label should be translated: {}",
            label
        );
    }

    // Test asymmetric tree
    let asymmetric_tree =
        result.tree("asymmetric").expect("Should have asymmetric tree");

    // Validate different structure
    assert_eq!(
        asymmetric_tree.tip_count_all(),
        4,
        "Asymmetric tree should have 4 tips"
    );
    assert_eq!(
        asymmetric_tree.node_count_all(),
        7,
        "Asymmetric tree should have 7 total nodes"
    );

    // Height should be 0.4 (longest path: 1 -> root = 0.4)
    let asym_height = calculate_tree_height(asymmetric_tree);
    assert!(
        (asym_height - 0.4).abs() < 1e-6,
        "Asymmetric tree height should be 0.4"
    );

    println!(
        "NEXUS trees validated: symmetric height={:.3}, asymmetric height={:.3}",
        height, asym_height
    );
}

// Helper structures and functions

#[derive(Debug)]
#[allow(dead_code)]
struct TreeTopology {
    tip_count: usize,
    internal_count: usize,
    total_count: usize,
    is_binary: bool,
    max_depth: usize,
}

#[derive(Debug)]
struct BranchLengthStats {
    has_any: bool,
    has_all: bool,
    min_length: Option<f64>,
    max_length: Option<f64>,
    total_length: f64,
}

fn validate_tree_topology(tree: &Tree, expected: &TreeTopology, name: &str) {
    assert_eq!(
        tree.tip_count_all(),
        expected.tip_count,
        "Tip count mismatch for {}: expected {}, got {}",
        name,
        expected.tip_count,
        tree.tip_count_all()
    );

    let internal_count = tree.internal_node_count_all();
    assert_eq!(
        internal_count, expected.internal_count,
        "Internal count mismatch for {}: expected {}, got {}",
        name, expected.internal_count, internal_count
    );

    assert_eq!(
        tree.node_count_all(),
        expected.total_count,
        "Total count mismatch for {}: expected {}, got {}",
        name,
        expected.total_count,
        tree.node_count_all()
    );

    let max_depth = calculate_max_depth(tree);
    assert_eq!(
        max_depth, expected.max_depth,
        "Max depth mismatch for {}: expected {}, got {}",
        name, expected.max_depth, max_depth
    );
}

fn calculate_tree_height(tree: &Tree) -> f64 {
    let root_id = tree.first_node_id().expect("Tree should have a root");
    calculate_node_height(tree, root_id)
}

fn calculate_node_height(tree: &Tree, node_id: dendros::NodeId) -> f64 {
    let children = tree.children(&node_id);
    if children.is_empty() {
        // Leaf node
        0.0
    } else {
        // Internal node: max child height + branch length to that child
        children
            .iter()
            .map(|child| {
                let child_id = child.node_id().expect("Child should have ID");
                let branch_len = child.branch_length().unwrap_or(0.0);
                calculate_node_height(tree, *child_id) + branch_len
            })
            .fold(0.0, f64::max)
    }
}

fn calculate_tip_distances(tree: &Tree) -> Vec<f64> {
    let mut distances = Vec::new();
    for &tip_id in tree.tip_node_ids_all().iter() {
        let distance = calculate_distance_to_root(tree, tip_id);
        distances.push(distance);
    }
    distances.sort_by(|a, b| a.partial_cmp(b).unwrap());
    distances
}

fn calculate_distance_to_root(tree: &Tree, node_id: dendros::NodeId) -> f64 {
    let mut current_id = node_id;
    let mut total_distance = 0.0;

    while let Some(parent_id) = tree.parent_id(&current_id) {
        if let Some(node) = tree.node(Some(current_id)) {
            total_distance += node.branch_length().unwrap_or(0.0);
        }
        current_id = *parent_id;
    }

    total_distance
}

fn calculate_max_depth(tree: &Tree) -> usize {
    let root_id = tree.first_node_id().expect("Tree should have a root");
    calculate_node_depth(tree, root_id, 0)
}

fn calculate_node_depth(
    tree: &Tree,
    node_id: dendros::NodeId,
    current_depth: usize,
) -> usize {
    let children = tree.children(&node_id);
    if children.is_empty() {
        current_depth
    } else {
        children
            .iter()
            .map(|child| {
                let child_id = child.node_id().expect("Child should have ID");
                calculate_node_depth(tree, *child_id, current_depth + 1)
            })
            .max()
            .unwrap_or(current_depth)
    }
}

fn calculate_branch_length_stats(tree: &Tree) -> BranchLengthStats {
    let mut lengths = Vec::new();
    let mut has_any = false;
    let mut total_length = 0.0;

    // Check all nodes for branch lengths (except root)
    for &node_id in tree.node_ids_all().iter() {
        if let Some(node) = tree.node(Some(node_id)) {
            if let Some(length) = node.branch_length() {
                lengths.push(length);
                has_any = true;
                total_length += length;
            }
        }
    }

    let has_all = if tree.node_count_all() <= 1 {
        true // Single node tree trivially has "all" branch lengths
    } else {
        lengths.len() == tree.node_count_all() - 1 // All nodes except root should have lengths
    };

    let min_length = if lengths.is_empty() {
        None
    } else {
        Some(lengths.iter().cloned().fold(f64::INFINITY, f64::min))
    };
    let max_length = if lengths.is_empty() {
        None
    } else {
        Some(lengths.iter().cloned().fold(f64::NEG_INFINITY, f64::max))
    };

    BranchLengthStats { has_any, has_all, min_length, max_length, total_length }
}
