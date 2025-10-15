use dendros::parse_newick;

#[test]
fn test_basic_polytomy_parsing() {
    let large_star_tree = generate_large_star_tree(5000);

    let test_cases = vec![
        // Basic polytomies
        (
            "Three-way polytomy",
            "(A,B,C);",
            PolytomyExpectation {
                tip_count: 3,
                total_nodes: 4,
                root_children: 3,
                is_binary: false,
                max_polytomy_size: 3,
            },
        ),
        (
            "Four-way polytomy",
            "(A,B,C,D);",
            PolytomyExpectation {
                tip_count: 4,
                total_nodes: 5,
                root_children: 4,
                is_binary: false,
                max_polytomy_size: 4,
            },
        ),
        (
            "Large star tree",
            &large_star_tree, // 5000-tip star tree = 5001 nodes total
            PolytomyExpectation {
                tip_count: 5000,
                total_nodes: 5001,
                root_children: 5000,
                is_binary: false,
                max_polytomy_size: 5000,
            },
        ),
        // Mixed binary and polytomies
        (
            "Binary with polytomy",
            "((A,B),(C,D,E));",
            PolytomyExpectation {
                tip_count: 5,
                total_nodes: 8,
                root_children: 2,
                is_binary: false,
                max_polytomy_size: 3,
            },
        ),
        (
            "Polytomy with binary",
            "(A,(B,C,D,E));",
            PolytomyExpectation {
                tip_count: 5,
                total_nodes: 7,
                root_children: 2,
                is_binary: false,
                max_polytomy_size: 4,
            },
        ),
        (
            "Complex mixed",
            "((A,B,C),(D,E),(F,G,H,I));",
            PolytomyExpectation {
                tip_count: 9,
                total_nodes: 13,
                root_children: 3,
                is_binary: false,
                max_polytomy_size: 4,
            },
        ),
    ];

    for (name, newick_str, expected) in test_cases {
        println!("Testing polytomy: {}", name);
        let trees = parse_newick(newick_str.to_string()).unwrap_or_else(|| {
            panic!("Failed to parse polytomy tree: {}", name)
        });

        let tree = &trees[0];
        validate_polytomy_expectations(tree, &expected, name);

        println!(
            "Polytomy validated: {} tips, max polytomy size {}",
            expected.tip_count, expected.max_polytomy_size
        );
    }
}

#[test]
fn test_polytomy_branch_length_handling() {
    let test_cases = vec![
        // Polytomies with branch lengths
        ("Polytomy equal branches", "(A:1.0,B:1.0,C:1.0,D:1.0);", 4, 1.0),
        ("Polytomy different branches", "(A:0.5,B:1.0,C:1.5,D:2.0);", 4, 2.0),
        (
            "Mixed with internal branches", "((A:0.1,B:0.1,C:0.1):0.5,D:0.6);",
            4, 0.6,
        ),
        (
            "Ultrametric polytomy",
            "((A:0.5,B:0.5,C:0.5):0.5,(D:0.5,E:0.5):0.5);", 5, 1.0,
        ),
        (
            "Complex polytomy tree",
            "((A:0.1,B:0.2,C:0.3):0.4,(D:0.2,E:0.3,F:0.4,G:0.5):0.1);", 7, 0.7,
        ),
    ];

    for (name, newick_str, expected_tips, expected_height) in test_cases {
        println!("Testing polytomy with branch lengths: {}", name);
        let trees = parse_newick(newick_str.to_string()).unwrap_or_else(|| {
            panic!("Failed to parse polytomy tree with branches: {}", name)
        });

        let tree = &trees[0];

        assert_eq!(
            tree.tip_count_all(),
            expected_tips,
            "Wrong tip count for {}",
            name
        );

        let actual_height = tree.height();
        assert!(
            (actual_height - expected_height).abs() < 1e-6,
            "Height mismatch for {}: expected {}, got {}",
            name,
            expected_height,
            actual_height
        );

        // Verify tree is not strictly binary if it has polytomies
        let has_polytomies = has_multifurcating_nodes(tree);
        if has_polytomies {
            println!("Confirmed polytomies present (non-binary tree)");
        }

        println!(
            "Height: {:.3}, Tips: {}",
            actual_height,
            tree.tip_count_all()
        );
    }
}

#[test]
fn test_polytomy_node_degree_analysis() {
    // Test that we can correctly identify and count nodes by degree
    let polytomy_cases = vec![
        ("Simple trifurcation", "(A,B,C);", vec![3]), // root has degree 3
        ("Mixed degrees", "((A,B),(C,D,E,F));", vec![2, 4]), // root=2, internal=4
        ("Complex", "(A,(B,C,D),(E,F),(G,H,I,J));", vec![4, 3, 2, 4]), // various degrees
    ];

    for (name, newick_str, expected_max_degrees) in polytomy_cases {
        println!("Testing node degrees: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];
        let degree_stats = analyze_node_degrees(tree);

        println!("  Node degree distribution:");
        for (degree, count) in &degree_stats.degree_counts {
            println!("    Degree {}: {} nodes", degree, count);
        }

        assert!(
            degree_stats.max_degree
                >= *expected_max_degrees.iter().max().unwrap(),
            "Expected max degree >= {} for {}",
            expected_max_degrees.iter().max().unwrap(),
            name
        );

        assert!(
            degree_stats.has_polytomies,
            "Tree {} should have polytomies",
            name
        );

        println!(
            "Max degree: {}, Polytomies: {}",
            degree_stats.max_degree, degree_stats.has_polytomies
        );
    }
}

#[test]
fn test_polytomy_binary_tree_relationship() {
    // Test that polytomies are preserved and not artificially resolved
    let resolution_cases = vec![
        ("Preserve trifurcation", "(A,B,C);", 3, 1), // 3 tips, 1 internal
        ("Preserve large polytomy", "(A,B,C,D,E);", 5, 1), // 5 tips, 1 internal
        ("Mixed preservation", "((A,B,C),D);", 4, 2), // 4 tips, 2 internals
    ];

    for (name, newick_str, expected_tips, expected_internals) in
        resolution_cases
    {
        println!("Testing polytomy preservation: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];

        assert_eq!(
            tree.tip_count_all(),
            expected_tips,
            "Tips count should be preserved for {}",
            name
        );
        assert_eq!(
            tree.internal_node_count_all(),
            expected_internals,
            "Internal node count should match for {}",
            name
        );

        // Ensure the binary tree formula l = i + 1 does NOT hold for polytomies
        let follows_binary_formula =
            tree.tip_count_all() == tree.internal_node_count_all() + 1;
        if expected_internals < expected_tips - 1 {
            assert!(
                !follows_binary_formula,
                "Polytomy tree {} should NOT follow binary formula",
                name
            );
            println!("Polytomy correctly preserved (non-binary)");
        } else {
            println!("Binary tree structure maintained");
        }
    }
}

// Helper structs and functions

#[derive(Debug)]
struct PolytomyExpectation {
    tip_count: usize,
    total_nodes: usize,
    root_children: usize,
    is_binary: bool,
    max_polytomy_size: usize,
}

#[derive(Debug)]
struct NodeDegreeStats {
    degree_counts: std::collections::HashMap<usize, usize>,
    max_degree: usize,
    has_polytomies: bool,
}

fn validate_polytomy_expectations(
    tree: &dendros::Tree,
    expected: &PolytomyExpectation,
    name: &str,
) {
    assert_eq!(
        tree.tip_count_all(),
        expected.tip_count,
        "Wrong tip count for {}: expected {}, got {}",
        name,
        expected.tip_count,
        tree.tip_count_all()
    );

    assert_eq!(
        tree.node_count_all(),
        expected.total_nodes,
        "Wrong total nodes for {}: expected {}, got {}",
        name,
        expected.total_nodes,
        tree.node_count_all()
    );

    if let Some(root_id) = tree.first_node_id() {
        let actual_root_children = tree.child_ids(&root_id).len();
        assert_eq!(
            actual_root_children, expected.root_children,
            "Wrong root children count for {}: expected {}, got {}",
            name, expected.root_children, actual_root_children
        );
    }

    let has_polytomies = has_multifurcating_nodes(tree);
    assert_eq!(
        has_polytomies, !expected.is_binary,
        "Binary expectation mismatch for {}: expected binary={}, has_polytomies={}",
        name, expected.is_binary, has_polytomies
    );
}

fn has_multifurcating_nodes(tree: &dendros::Tree) -> bool {
    let all_node_ids = tree.node_ids_all();
    for node_id in all_node_ids {
        let child_count = tree.child_ids(&node_id).len();
        if child_count > 2 {
            return true;
        }
    }
    false
}

fn analyze_node_degrees(tree: &dendros::Tree) -> NodeDegreeStats {
    let mut degree_counts = std::collections::HashMap::new();
    let mut max_degree = 0;

    let all_node_ids = tree.node_ids_all();
    for node_id in all_node_ids {
        let degree = tree.child_ids(&node_id).len();
        *degree_counts.entry(degree).or_insert(0) += 1;
        max_degree = max_degree.max(degree);
    }

    let has_polytomies = degree_counts.keys().any(|&degree| degree > 2);

    NodeDegreeStats { degree_counts, max_degree, has_polytomies }
}

/// Generate a large star tree NEWICK string with the specified number of tips
fn generate_large_star_tree(n_tips: usize) -> String {
    let mut tips = Vec::with_capacity(n_tips);
    for i in 1..=n_tips {
        tips.push(format!("T{}", i));
    }
    format!("({});", tips.join(","))
}
